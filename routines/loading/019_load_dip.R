# 014_load_dip.R
# Loads direct investment position data from IMF DIP (formerly CDIS)
# Finflows 3.0
# Memory-safe: processes countries one at a time, immediately
# saves to cache and frees memory after each query.
# Safe to interrupt and re-run: cached countries are skipped.
# -----------------------------------------------------------
# DIP has 5 dimensions (excl TIME):
#   1. COUNTRY            — reporting country (ISO3)
#   2. DV_TYPE            — O (official reported), SCC (derived/mirror)
#   3. INDICATOR          — 20 codes (direction+gross/net+instrument+entity)
#   4. COUNTERPART_COUNTRY — counterpart country (ISO3)
#   5. FREQ               — A, S, Q (requested subset)
# -----------------------------------------------------------

rm(list = ls())
gc(full = TRUE, reset = TRUE)
library(MDstats); library(MD3)
defaultcountrycode(NULL)

# --- Setup buffer directory and log ---
# --- Setup buffer directory and log ---
if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/loaded'

if (!dir.exists(file.path(data_dir,"dipbuffer"))) dir.create(file.path(data_dir,"dipbuffer"), recursive = TRUE)
sink(file.path(data_dir,"dipbuffer","diploader.log"), append = FALSE)
cat(gc(), '\n')

# --- Indicators to request ---
vindicators <- paste(
  "OTWD_D_AG_FALL_FE", "OTWD_D_AG_FL_ALL",
  "INWD_D_AG_FALL_FE", "INWD_D_AG_FL_ALL",
  "OTWD_D_LG_FALL_FE", "OTWD_D_LG_FL_ALL",
  "INWD_D_LG_FALL_FE", "INWD_D_LG_FL_ALL",
  "OTWD_D_NETAL_FALL_ALL", "OTWD_D_NETAL_FALL_FE",
  "OTWD_D_NETAL_FL_ALL",   "OTWD_D_NETAL_FL_RFI",
  "OTWD_D_NETAL_FL_REXFI", "OTWD_D_NETAL_F51_ALL",
  "INWD_D_NETLA_FALL_ALL", "INWD_D_NETLA_FALL_FE",
  "INWD_D_NETLA_FL_ALL",   "INWD_D_NETLA_FL_RFI",
  "INWD_D_NETLA_FL_REXFI", "INWD_D_NETLA_F51_ALL",
  sep = "+"
)

# --- Frequencies to request ---
vfreq <- "A+S+Q"

# --- Get list of reporting countries ---
if (!file.exists(file.path(data_dir,"dipbuffer","whatctries_dip.rds"))) {
  cat('Querying reporting country list from IMF/DIP...\n')
  whatctries <- mds('IMF/DIP/.O.OTWD_D_NETAL_FALL_ALL.USA.', labels = FALSE)
  saveRDS(whatctries, file.path(data_dir,"dipbuffer","whatctries_dip.rds"))
} else {
  whatctries <- readRDS(file.path(data_dir,"dipbuffer","whatctries_dip.rds"))
}
ccc <- dimcodes(whatctries)[[1]]
rm(whatctries); gc(full = TRUE, reset = TRUE)

# --- Status tracking ---
# USA is added explicitly: absent from ccc by construction
statusvec <- setNames(rep(NA_character_, NROW(ccc) + 1L), c(ccc[, 1], 'USA'))
dimlog    <- vector('list', NROW(ccc) + 1L); names(dimlog) <- c(ccc[, 1], 'USA')

# --- Main loading loop ---
cat('\nLoading DIP data for ', NROW(ccc), ' reporting countries\n')

for (cc in ccc[, 1]) {

  cat('\n___ ', as.character(Sys.time()), ': country ',
      match(cc, ccc[, 1]), '/', NROW(ccc), ': ', cc, ' ___')

  cachefile <- file.path(data_dir,"dipbuffer", 'dip_' %&% cc %&% '.rds')

  if (file.exists(cachefile)) {
    cat(' cache... ')
    temp <- readRDS(cachefile)
    statusvec[cc] <- 'L'
    dimlog[[cc]] <- dim(temp)
    cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
    rm(temp)

  } else {
    cat(' querying IMF... ')
    gc(full = TRUE, reset = TRUE)

    temp <- try(
      mds('IMF/DIP/' %&% cc %&% '.O+SCC.' %&% vindicators %&% '..' %&% vfreq,
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

    rm(temp)
    gc(full = TRUE, reset = TRUE)
  }
}

# --- USA: special case (absent from ccc by construction) ---
cat('\n___ ', as.character(Sys.time()), ': USA (special case) ___')

cachefile <- file.path(data_dir,"dipbuffer","dip_USA.rds")

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
    mds('IMF/DIP/USA.O+SCC.' %&% vindicators %&% '..' %&% vfreq,
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
cat('\n\n', as.character(Sys.time()), 'DIP loading results\n')
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
saveRDS(statustable, file.path(data_dir,"dipbuffer","resultstable.rds"))

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
cachefiles <- dir(file.path(data_dir,"dipbuffer"), pattern = '^dip_.*\\.rds$', full.names = TRUE)
ldip <- vector('list', length(cachefiles))
names(ldip) <- gsub('^dip_|\\.rds$', '', basename(cachefiles))
for (i in seq_along(cachefiles)) {
  ldip[[i]] <- readRDS(cachefiles[i])
}
saveRDS(ldip, file.path(data_dir, 'alldipresults.rds'))
rm(ldip); gc(full = TRUE, reset = TRUE)
cat('Done. Combined results saved to ', file.path(data_dir, 'alldipresults.rds'), '\n')
cat(nI + nL, ' countries loaded successfully,  ', n0, ' empty,  ', nX, ' errors\n')
