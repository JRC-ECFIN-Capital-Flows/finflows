# 016_load_bop.R
# Loads balance of payments (financial account) data from IMF BOP
# Finflows 3.0
# Memory-safe: processes countries one at a time, immediately
# saves to cache and frees memory after each query.
# Safe to interrupt and re-run: cached countries are skipped.
# -----------------------------------------------------------
# BOP has 4 dimensions (excl TIME):
#   1. COUNTRY              — reporting country (ISO3)
#   2. BOP_ACCOUNTING_ENTRY — A_NFA_T (net acquisition of financial assets),
#                             L_NIL_T (net incurrence of liabilities)
#   3. INDICATOR            — financial account instrument tree (all requested)
#   4. UNIT                 — USD only
# TIME mixes annual ("2023") and quarterly ("2023q1") codes;
# quarterly filtering is deferred to the filling script.
# -----------------------------------------------------------
# Unlike PIP/DIP, BOP is non-bilateral (no counterpart country
# dimension), so there is no USA special case — the reporter
# probe returns all reporters including USA directly.
# -----------------------------------------------------------

rm(list = ls())
gc(full = TRUE, reset = TRUE)
library(MDstats); library(MD3)
defaultcountrycode(NULL)

# --- Setup buffer directory and log ---
if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/loaded'

if (!dir.exists(file.path(data_dir, "imfbopbuffer"))) dir.create(file.path(data_dir, "imfbopbuffer"), recursive = TRUE)
sink(file.path(data_dir, "imfbopbuffer", "boploader.log"), append = FALSE)
cat(gc(), '\n')

# --- Accounting entries to request ---
# A_NFA_T = net acquisition of financial assets (transactions)
# L_NIL_T = net incurrence of liabilities (transactions)
vacctentry <- "A_NFA_T+L_NIL_T"

# --- Get list of reporting countries ---
# Probe: blank COUNTRY returns all reporters; pin accounting entry and unit
# to keep the probe lightweight. Indicator left blank = all.
if (!file.exists(file.path(data_dir, "imfbopbuffer", "whatctries_bop.rds"))) {
  cat('Querying reporting country list from IMF_DATA/BOP...\n')
  whatctries <- mds('IMF_DATA/BOP/.A_NFA_T..USD', labels = FALSE)
  saveRDS(whatctries, file.path(data_dir, "imfbopbuffer", "whatctries_bop.rds"))
} else {
  whatctries <- readRDS(file.path(data_dir, "imfbopbuffer", "whatctries_bop.rds"))
}
ccc <- dimcodes(whatctries)[[1]]
rm(whatctries); gc(full = TRUE, reset = TRUE)

# --- Status tracking ---
statusvec <- setNames(rep(NA_character_, NROW(ccc)), ccc[, 1])
dimlog    <- vector('list', NROW(ccc)); names(dimlog) <- ccc[, 1]

# --- Main loading loop ---
cat('\nLoading BOP data for ', NROW(ccc), ' reporting countries\n')

for (cc in ccc[, 1]) {
  
  cat('\n___ ', as.character(Sys.time()), ': country ',
      match(cc, ccc[, 1]), '/', NROW(ccc), ': ', cc, ' ___')
  
  cachefile <- file.path(data_dir, "imfbopbuffer", paste0("bop_", cc, ".rds"))
  
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
    
    # Key: COUNTRY.BOP_ACCOUNTING_ENTRY.INDICATOR.UNIT
    # Indicator left blank = all available indicators
    temp <- try(
      mds('IMF_DATA/BOP/' %&% cc %&% '.' %&% vacctentry %&% '..USD',
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

# --- Reporting ---
cat('\n\n', as.character(Sys.time()), 'BOP loading results\n')
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
saveRDS(statustable, file.path(data_dir, "imfbopbuffer", "resultstable.rds"))

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
cachefiles <- dir(file.path(data_dir, "imfbopbuffer"), pattern = '^bop_.*\\.rds$', full.names = TRUE)
lbop <- vector('list', length(cachefiles))
names(lbop) <- gsub('^bop_|\\.rds$', '', basename(cachefiles))
for (i in seq_along(cachefiles)) {
  lbop[[i]] <- readRDS(cachefiles[i])
}
saveRDS(lbop, file.path(data_dir, "imfbopbuffer", "allimfbopresults.rds"))
saveRDS(lbop, file.path(data_dir, 'vintages/allimfbopresults_' %&% format(Sys.Date(), "%F") %&% '.rds'))
rm(lbop); gc(full = TRUE, reset = TRUE)
cat('Done. Combined results saved to data/loaded/imfbopbuffer/allimfbopresults.rds\n')
cat(nI + nL, ' countries loaded successfully,  ', n0, ' empty,  ', nX, ' errors\n')