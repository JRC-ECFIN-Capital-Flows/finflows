# 014_load_pip.R
# Loads portfolio investment data from IMF PIP (formerly CPIS)
# Finflows 3.0
# Memory-safe: processes countries one at a time, immediately
# saves to cache and frees memory after each query.
# Safe to interrupt and re-run: cached countries are skipped.
# -----------------------------------------------------------

if (!exists("data_dir")) data_dir = getwd()

rm(list = ls())
gc(full = TRUE, reset = TRUE)
library(MDstats); library(MD3)
defaultcountrycode(NULL)

# --- Setup buffer directory and log ---
if (!dir.exists(file.path(data_dir,"pipbuffer"))) dir.create(file.path(data_dir,"pipbuffer"), recursive = TRUE)
sink(file.path(data_dir,"pipbuffer","piploader.log"), append = FALSE)
cat(gc(), '\n')

# --- Indicators to request ---
# 5 indicators: total, equity, debt total, debt short, debt long
vindicators <- "P_TOTINV_P_USD+P_F51_P_USD+P_F3_P_USD+P_F3_S_P_USD+P_F3_L_P_USD"

# --- Get list of reporting countries ---
# Query: all countries, assets, debt total, sector=S1, counterpart_country=USA, all freq
if (!file.exists(file.path(data_dir,"pipbuffer","whatctries_pip.rds"))) {
  cat('Querying reporting country list from IMF/PIP...\n')
  whatctries <- mds('IMF/PIP/.A.P_F3_P_USD.S1..USA.', labels = FALSE)
  saveRDS(whatctries, file.path(data_dir,"pipbuffer","whatctries_pip.rds"))
} else {
  whatctries <- readRDS(file.path(data_dir,"pipbuffer","whatctries_pip.rds"))
}
ccc <- dimcodes(whatctries)[[1]]
rm(whatctries); gc(full = TRUE, reset = TRUE)

# --- Status tracking ---
# USA is added explicitly: by construction it is absent from ccc
# (the country list was built from reporters holding assets vs USA)
statusvec <- setNames(rep(NA_character_, NROW(ccc) + 1L), c(ccc[, 1], 'USA'))
dimlog    <- vector('list', NROW(ccc) + 1L); names(dimlog) <- c(ccc[, 1], 'USA')

# --- Main loading loop ---
cat('\nLoading PIP data for ', NROW(ccc), ' reporting countries\n')

for (cc in ccc[, 1]) {
  
  cat('\n___ ', as.character(Sys.time()), ': country ',
      match(cc, ccc[, 1]), '/', NROW(ccc), ': ', cc, ' ___')
  
  cachefile <- file.path(data_dir, "pipbuffer", paste0("pip_", cc, ".rds"))
  
  if (file.exists(cachefile)) {
    # --- Load dims from cache (read, record dims, free immediately) ---
    cat(' cache... ')
    temp <- readRDS(cachefile)
    statusvec[cc] <- 'L'
    dimlog[[cc]] <- dim(temp)
    cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
    rm(temp)
    
  } else {
    # --- Query IMF ---
    cat(' querying IMF... ')
    
    # Force full garbage collection before each IMF query
    # to ensure maximum memory available for XML parsing
    gc(full = TRUE, reset = TRUE)
    
    temp <- try(
      mds('IMF/PIP/' %&% cc %&% '.A+L.' %&% vindicators %&% '....', 
          drop = FALSE, labels = FALSE),
      silent = TRUE
    )
    
    if (is(temp, 'md3')) {
      statusvec[cc] <- 'I'
      dimlog[[cc]] <- dim(temp)
      saveRDS(temp, cachefile)
      cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
      
    } else if (any(grepl('err', class(temp)))) {
      msg <- as.character(temp)
      if (any(grepl('SDMX result contains 0 time series', msg, ignore.case = TRUE))) {
        statusvec[cc] <- '0'
        cat('no data\n')
      } else {
        statusvec[cc] <- 'X'
        cat('ERROR: ', substr(msg[1], 1, 120), '\n')
        Sys.sleep(3)
      }
    }
    
    # Free immediately and force full GC
    rm(temp)
    gc(full = TRUE, reset = TRUE)
  }
}

# --- USA: special case (absent from ccc by construction) ---
cat('\n___ ', as.character(Sys.time()), ': USA (special case) ___')

cachefile <- file.path(data_dir, "pipbuffer", "pip_USA.rds")

if (file.exists(cachefile)) {
  cat(' cache... ')
  temp <- readRDS(cachefile)
  statusvec['USA'] <- 'L'
  dimlog[['USA']] <- dim(temp)
  cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
  rm(temp)
  
} else {
  cat(' querying IMF... ')
  gc(full = TRUE, reset = TRUE)
  
  temp <- try(
    mds('IMF/PIP/USA.A+L.' %&% vindicators %&% '....', 
        drop = FALSE, labels = FALSE),
    silent = TRUE
  )
  
  if (is(temp, 'md3')) {
    statusvec['USA'] <- 'I'
    dimlog[['USA']] <- dim(temp)
    saveRDS(temp, cachefile)
    cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
    
  } else if (any(grepl('err', class(temp)))) {
    msg <- as.character(temp)
    if (any(grepl('SDMX result contains 0 time series', msg, ignore.case = TRUE))) {
      statusvec['USA'] <- '0'
      cat('no data\n')
    } else {
      statusvec['USA'] <- 'X'
      cat('ERROR: ', substr(msg[1], 1, 120), '\n')
      Sys.sleep(3)
    }
  }
  
  rm(temp)
  gc(full = TRUE, reset = TRUE)
}

# --- Reporting ---
cat('\n\n', as.character(Sys.time()), 'PIP loading results\n')
cat('  I = loaded from IMF\n')
cat('  L = loaded from local cache\n')
cat('  0 = no series available from IMF\n')
cat('  X = server loading error\n\n')

statustable <- data.frame(
  COUNTRY = names(statusvec),
  STATUS  = statusvec,
  stringsAsFactors = FALSE
)
print(statustable)
saveRDS(statustable, file.path(data_dir,"pipbuffer","resultstable.rds"))

# --- Dimension summary for successful loads ---
cat('\n\nDimension summary for successfully loaded countries:\n')
loaded <- statusvec %in% c('I', 'L')
if (any(loaded)) {
  dimsummary <- do.call(rbind, dimlog[loaded])
  print(dimsummary)
}

# --- Count failures ---
nI <- sum(statusvec == 'I', na.rm = TRUE)
nL <- sum(statusvec == 'L', na.rm = TRUE)
n0 <- sum(statusvec == '0', na.rm = TRUE)
nX <- sum(statusvec == 'X', na.rm = TRUE)
cat('\nLoaded: ', nI, ' from IMF, ', nL, ' from cache\n')
cat('Empty:  ', n0, '\n')
cat('Errors: ', nX, '\n')

if (nX > 0) {
  cat('\nFailed countries: ', paste(names(statusvec[statusvec == 'X']), collapse = ', '), '\n')
  cat('To retry, delete their cache files and re-run this script.\n')
}

sink()

# --- Build combined results from cache ---
cat('Building combined results from cache files...\n')
gc(full = TRUE, reset = TRUE)
cachefiles <- dir(file.path(data_dir, "pipbuffer"), pattern = '^pip_.*\\.rds$', full.names = TRUE)
lpip <- vector('list', length(cachefiles))
names(lpip) <- gsub('^pip_|\\.rds$', '', basename(cachefiles))
for (i in seq_along(cachefiles)) {
  lpip[[i]] <- readRDS(cachefiles[i])
}
saveRDS(lpip, file.path(data_dir,"pipbuffer","allpipresults.rds"))
rm(lpip); gc(full = TRUE, reset = TRUE)
cat('Done. Combined results saved to data/loaded/pipbuffer/allpipresults.rds\n')
cat(nI + nL, ' countries loaded successfully,  ', n0, ' empty,  ', nX, ' errors\n')
