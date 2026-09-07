# 017_load_iip.R
# Loads international investment position data from IMF IIP
# Finflows 3.0
# Memory-safe: processes countries one at a time, immediately
# saves to cache and frees memory after each query.
# Safe to interrupt and re-run: cached countries are skipped.
# -----------------------------------------------------------
# IIP is served from provider IMF_DATA (NOT IMF, unlike PIP/DIP).
#
# The DSD (DSD_BOP, shared with the BOP dataflow) declares five
# dimensions excl. TIME, including FREQ. Both a 5-field key
# (ending .Q) and a 4-field key are accepted by the SDMX endpoint:
#   - 5-field with .Q  -> quarterly observations only
#   - 4-field          -> annual AND quarterly, mixed in TIME
#     (annual codes look like "2004", quarterly like "2004q4")
# Verified identical values for overlapping periods.
#
# We use the 4-FIELD key and cache the full history, because
# quarterly coverage starts late for several reporters (e.g. USA
# only from 2005q4, while annual data go back to 1976). The filler
# uses quarterly data first and then fills remaining NAs with the
# annual observations, mapped onto the q4 of the reference year.
#
# Key: COUNTRY.BOP_ACCOUNTING_ENTRY.INDICATOR.UNIT
#   1. COUNTRY              - reporting country (ISO3)
#   2. BOP_ACCOUNTING_ENTRY - A_P (assets, positions),
#                             L_P (liabilities, positions)
#   3. INDICATOR            - instrument tree (all requested; the
#                             selection is made in the filler via
#                             the mapping table)
#   4. UNIT                 - USD only
# -----------------------------------------------------------
# Unlike PIP/DIP, IIP is non-bilateral (no counterpart country
# dimension), so there is no USA special case - the reporter
# probe returns all reporters including USA directly.
# -----------------------------------------------------------

rm(list = ls())
gc(full = TRUE, reset = TRUE)
library(MDstats); library(MD3)
defaultcountrycode(NULL)

`%&%` = function (..., collapse = NULL, recycle0 = FALSE)  .Internal(paste0(list(...), collapse, recycle0))

# --- Setup buffer directory and log ---
if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/loaded'

if (!dir.exists(file.path(data_dir, "imfiipbuffer"))) dir.create(file.path(data_dir, "imfiipbuffer"), recursive = TRUE)
sink(file.path(data_dir, "imfiipbuffer", "iiploader.log"), append = FALSE)
cat(gc(), '\n')

# --- Accounting entries to request ---
vacctentry <- "A_P+L_P"

# --- Get list of reporting countries ---
if (!file.exists(file.path(data_dir, "imfiipbuffer", "whatctries_iip.rds"))) {
  cat('Querying reporting country list from IMF_DATA/IIP...\n')
  whatctries <- mds('IMF_DATA/IIP/.A_P..USD', labels = FALSE)
  saveRDS(whatctries, file.path(data_dir, "imfiipbuffer", "whatctries_iip.rds"))
} else {
  whatctries <- readRDS(file.path(data_dir, "imfiipbuffer", "whatctries_iip.rds"))
}
ccc <- dimcodes(whatctries)[[1]]
rm(whatctries); gc(full = TRUE, reset = TRUE)

# --- Status tracking ---
statusvec <- setNames(rep(NA_character_, NROW(ccc)), ccc[, 1])
dimlog    <- vector('list', NROW(ccc)); names(dimlog) <- ccc[, 1]

# --- Main loading loop ---
cat('\nLoading IIP data for ', NROW(ccc), ' reporting countries\n')

for (cc in ccc[, 1]) {
  
  cat('\n___ ', as.character(Sys.time()), ': country ',
      match(cc, ccc[, 1]), '/', NROW(ccc), ': ', cc, ' ___')
  
  cachefile <- file.path(data_dir, "imfiipbuffer", paste0("iip_", cc, ".rds"))
  
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
    
    # Key: COUNTRY.BOP_ACCOUNTING_ENTRY.INDICATOR.UNIT
    # Indicator left blank = all available indicators
    # No FREQ field and no startPeriod = full annual + quarterly history
    temp <- try(
      mds('IMF_DATA/IIP/' %&% cc %&% '.' %&% vacctentry %&% '..USD',
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

# --- Reporting ---
cat('\n\n', as.character(Sys.time()), 'IIP loading results\n')
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
saveRDS(statustable, file.path(data_dir, "imfiipbuffer", "resultstable.rds"))

cat('\n\nDimension summary for successfully loaded countries:\n')
loaded <- statusvec %in% c('I', 'L')
if (any(loaded)) {
  dimsummary <- do.call(rbind, dimlog[loaded])
  print(dimsummary)
}

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
cachefiles <- dir(file.path(data_dir, "imfiipbuffer"), pattern = '^iip_.*\\.rds$', full.names = TRUE)
liip <- vector('list', length(cachefiles))
names(liip) <- gsub('^iip_|\\.rds$', '', basename(cachefiles))
for (i in seq_along(cachefiles)) {
  liip[[i]] <- readRDS(cachefiles[i])
}
saveRDS(liip, file.path(data_dir, "imfiipbuffer", "allimfiipresults.rds"))
saveRDS(liip, file.path(data_dir, 'vintages/allimfiipresults_' %&% format(Sys.Date(), "%F") %&% '.rds'))
rm(liip); gc(full = TRUE, reset = TRUE)
cat('Done. Combined results saved to data/loaded/imfiipbuffer/allimfiipresults.rds\n')
cat(nI + nL, ' countries loaded successfully,  ', n0, ' empty,  ', nX, ' errors\n')