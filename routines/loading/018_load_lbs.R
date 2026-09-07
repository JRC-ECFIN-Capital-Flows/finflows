# 016_load_bis_lbs.R
# 
# Loads MFI loans and deposits data from BIS Locational Banking Statistics
# (Stocks and Flows, cross-country)
# Finflows 3.0
# -----------------------------------------------------------
#
#
# Dimensions (12 excl TIME):
# Frequency . Measure . Balance sheet position . Type of instruments . Currency denomination
# Currency type of reporting country . Parent country . Type of reporting institutions
# Reporting country . Counterparty sector . Counterparty country . Position type
#
# 1  FREQ:              Q (Quarterly)
# 2  L_MEASURE:         B (Break in stocks, i.e valuation effects) ; F (FX and break adjusted change); G (Growth); S (Stocks)
# 3  L_POSITION:        C ("Claims" -  assets); L (Liabilities)
# 4  L_INSTR:           A ("All" - F); B ("Credit" - F3+F4); D (F3); G (F2M+F4); I (F7+F89);
#                      M (F3S); U (Unallocated)
# 5  L_DENOM:           TO1 (all currencies); EUR; USD; CHF etc.
# 6  L_CURR_TYPE:       D (Domestic); F (Foreign); U (Unclassified); A (D+F+U)
# 7  L_PARENT_CTY:      varies, 5J for total, iso2
# 8  L_REP_BANK_TYPE:   A (All); B (Foreign); D (Domestic); S (Foreign subs.)
# 9  L_REP_CTY:         varies, 5A for total, 5C for EA, otherwise iso2
# 10 L_CP_SECTOR:       A (S1); B (S12K); C (S11); F (S12 excl. S12K); G (S13)
#                       H (S1M); M (S121); plus other arbitrary categories
# 11 L_CP_COUNTRY:      iso2, and various aggregates - 5C (EA); 5J (All)
# 12 L_POS_TYPE:        A (All); N (Cross-border); R (Local); U (Unallocated)
# -----------------------------------------------------------

rm(list = ls())
gc(full = TRUE, reset = TRUE)
library(MDstats); library(MD3)
defaultcountrycode(NULL)

# --- Setup buffer directory and log ---
if (!dir.exists(file.path(data_dir,"lbsbuffer"))) dir.create(file.path(data_dir,"lbsbuffer"), recursive = TRUE)
sink(file.path(data_dir,"lbsbuffer","lbsloader.log"), append = FALSE)
cat(gc(), '\n')

# --- Dataflow definition (single dataflow) ---
dfprefix <- "BIS/WS_LBS_D_PUB"
vdimsel  <- ".S..A+D+G+I+M.TO1.A.5J.A.%s...A+N+R"   # %s = slot 9 L_REP_CTY



# --- Get list of reporting countries ---
## Only included a few dimensions when retrieving country list (those that we will filter out anyway)
## If i leave everything blank, it doesn't run
if (!file.exists(file.path(data_dir,"lbsbuffer","whatctries_lbs.rds"))) {
  cat('Querying reporting country list from BIS LBS...\n')
  whatctries <- try(
    mds(dfprefix %&% '/.S...TO1.A.5J.A....',
        labels = FALSE),
    silent = TRUE
  )
  if (is(whatctries, 'md3')) {
    saveRDS(whatctries, file.path(data_dir,"lbsbuffer","whatctries_lbs.rds"))
  } else {
    cat('ERROR discovering countries: ', as.character(whatctries), '\n')
    sink()
    stop('Could not discover reporting countries. Check query.')
  }
} else {
  whatctries <- readRDS(file.path(data_dir,"lbsbuffer","whatctries_lbs.rds"))
}
ccc <- dimcodes(whatctries)[['L_REP_CTY']]
rm(whatctries); gc(full = TRUE, reset = TRUE)

# --- Status tracking ---
statusmat <- matrix(NA_character_,
                    nrow = NROW(ccc), ncol = 1,
                    dimnames = list(ccc[, 1], 'lbs'))

# --- Main loading loop (single dataflow, one file per country) ---
cat('\nLoading lbs data for ', NROW(ccc), ' reporting countries\n')

for (cc in ccc[, 1]) {
  
  cat('\n___ ', as.character(Sys.time()), ': country ',
      match(cc, ccc[, 1]), '/', NROW(ccc), ': ', cc, ' ___\n')
  
  cachefile <- file.path(data_dir, "lbsbuffer", paste0("lbs_", cc, ".rds"))

  if (file.exists(cachefile)) {
    cat('  cache... ')
    temp <- readRDS(cachefile)
    statusmat[cc, 'lbs'] <- 'L'
    cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
    rm(temp)
    
  } else {
    cat('  querying BIS... ')
    gc(full = TRUE, reset = TRUE)
    
    qry  <- dfprefix %&% '/' %&% sprintf(vdimsel, cc)
    temp <- try(mds(qry, drop = FALSE, labels = FALSE), silent = TRUE)
    
    if (is(temp, 'md3')) {
      statusmat[cc, 'lbs'] <- 'I'
      saveRDS(temp, cachefile)
      cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
      
    } else if (any(grepl('err', class(temp)))) {
      msg <- as.character(temp)
      if (any(grepl('0 time series|No results|404', msg, ignore.case = TRUE))) {
        statusmat[cc, 'lbs'] <- '0'
        cat('no data\n')
      } else {
        statusmat[cc, 'lbs'] <- 'X'
        cat('ERROR: ', substr(msg[1], 1, 120), '\n')
        Sys.sleep(3)
      }
    }
    
    rm(temp)
    gc(full = TRUE, reset = TRUE)
  }
}

# --- Reporting ---
cat('\n\n', as.character(Sys.time()), ': lbs loading results\n')
cat('  I = loaded from BIS\n')
cat('  L = loaded from local cache\n')
cat('  0 = no series available from BIS\n')
cat('  X = server loading error\n\n')

print(statusmat)
saveRDS(statusmat,file.path(data_dir,"lbsbuffer","statusmat.rds"))

nI <- sum(statusmat == 'I', na.rm = TRUE)
nL <- sum(statusmat == 'L', na.rm = TRUE)
n0 <- sum(statusmat == '0', na.rm = TRUE)
nX <- sum(statusmat == 'X', na.rm = TRUE)
cat('\nSummary: loaded', nI, 'from BIS,', nL, 'from cache,', n0, 'empty,', nX, 'errors\n')

if (nX > 0) {
  cat('\nFailed countries (delete cache file and re-run to retry):\n')
  for (cc in rownames(statusmat)[statusmat[, 'lbs'] == 'X']) cat(' ', cc, '\n')
}

sink()

# --- Build single combined result from all country cache files ---
cat('Building combined result from cache files...\n')
gc(full = TRUE, reset = TRUE)

cachefiles <- dir(file.path(data_dir,"lbsbuffer"), pattern = '^lbs_.*\\.rds$', full.names = TRUE)

if (length(cachefiles) > 0) {
  llbs <- vector('list', length(cachefiles))
  names(llbs) <- gsub('^lbs_|\\.rds$', '', basename(cachefiles))
  for (i in seq_along(cachefiles)) llbs[[i]] <- readRDS(cachefiles[i])
  saveRDS(llbs, file.path(data_dir,"lbsbuffer","alllbs.rds"))
  cat('Done:', length(llbs), 'countries combined -> data/loaded/lbsbuffer/alllbs.rds\n')
  rm(llbs); gc(full = TRUE, reset = TRUE)
} else {
  cat('No cache files found.\n')
}
