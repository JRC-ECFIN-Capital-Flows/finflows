# 015_load_fdi.R
# Loads FDI data from OECD (positions and flows, aggr and ctry)
# Finflows 3.0
# -----------------------------------------------------------
#
#
# Dimensions (13 excl TIME):
#   REF_AREA . MEASURE . UNIT_MEASURE . MEASURE_PRINCIPLE . ACCOUNTING_ENTRY .
#   TYPE_ENTITY . FDI_COMP . SECTOR . COUNTERPART_AREA . LEVEL_COUNTERPART .
#   ACTIVITY . FREQ . FDI_COLLECTION_ID
#
# Differences across dataflows:
#   MEASURE:           POS = 3 codes (LE_FA_F, LE_FA_F5, LE_FA_FL)
#                      FLOW_AGGR = 4 codes (T_FA_F, T_FA_F5A, T_FA_F5B, T_FA_FL)
#                      FLOW_CTRY = 3 usable codes (T_FA_F, T_FA_F5A, T_FA_FL)
#                        T_FA_F5B causes MD3 time-parsing error
#                        T_FA_F5AM/T_FA_F5AM_C: no data or time-parsing error
#   MEASURE_PRINCIPLE: AGGR = A,DI,DO,_Z; CTRY = DI,DO only (no A!)
#   FDI_COMP:          AGGR = 6 codes; CTRY = D only
#   LEVEL_COUNTERPART: POS_CTRY = IMC+ULT; others = IMC only
#   FREQ:              FLOW_AGGR = A+Q; others = A only
#   ACCOUNTING_ENTRY:  FLOW_CTRY = A,L,DIV,INV,NET; others = A,L,NET
#   UNIT_MEASURE:      varies but USD_EXC present in all
#   TYPE_ENTITY:       ALL,ROU,RSP in all
#   SECTOR:            S1 only in all
#   COUNTERPART_AREA:  469 codes in all
#   REF_AREA:          469 codes in all
#   FDI_COLLECTION_ID: AGGR in AGGR; CTRY_IND in CTRY (one per dataflow)
# -----------------------------------------------------------

rm(list = ls())
gc(full = TRUE, reset = TRUE)
library(MDstats); library(MD3)
defaultcountrycode(NULL)

# --- Setup buffer directory and log ---
if (!dir.exists(file.path(data_dir,"fdibuffer"))) dir.create(file.path(data_dir,"fdibuffer"), recursive = TRUE)
sink(file.path(data_dir,"fdibuffer","fdiloader.log"), append = FALSE)
cat(gc(), '\n')

# --- Dataflow definitions ---
dfprefix <- "OECD/OECD.DAF.INV,DSD_FDI@DF_FDI_"
dataflows <- c(
  pos_aggr  = "POS_AGGR",
  pos_ctry  = "POS_CTRY",
  flow_aggr = "FLOW_AGGR",
  flow_ctry = "FLOW_CTRY"
)

# --- Dimension selectors (all verified) ---
#
# Common selections across all dataflows:
#   UNIT_MEASURE:       USD_EXC
#   ACCOUNTING_ENTRY:   A+L
#   TYPE_ENTITY:        ALL+ROU+RSP
#   FDI_COMP:           D
#   SECTOR:             open (S1 only everywhere)
#   COUNTERPART_AREA:   open (all 469)
#   LEVEL_COUNTERPART:  open (IMC everywhere; IMC+ULT in POS_CTRY)
#   ACTIVITY:           open (_T only in AGGR/CTRY)
#   FREQ:               A
#   FDI_COLLECTION_ID:  open (one valid code per dataflow)
#
# Dataflow-specific:
#   MEASURE + MEASURE_PRINCIPLE differ — 3 distinct selectors needed.

# POS_AGGR: 3 position measures, asset/liability principle
vdimsel_pos_aggr <- ".LE_FA_F+LE_FA_F5+LE_FA_FL.USD_EXC.A.A+L.ALL+ROU+RSP.D.....A."

# POS_CTRY: 3 position measures, directional principle
vdimsel_pos_ctry <- ".LE_FA_F+LE_FA_F5+LE_FA_FL.USD_EXC.DI+DO.A+L.ALL+ROU+RSP.D.....A."

# FLOW_AGGR: 4 flow measures (incl T_FA_F5B, for Gabor, didnt cause problems with Hungary), asset/liability principle
vdimsel_flow_aggr <- ".T_FA_F+T_FA_F5A+T_FA_F5B+T_FA_FL.USD_EXC.A.A+L.ALL+ROU+RSP.D.....A."

# FLOW_CTRY: 3 flow measures (T_FA_F5B excluded: MD3 time-parsing error;
#   T_FA_F5AM/T_FA_F5AM_C excluded: no data or time-parsing error), directional principle
vdimsel_flow_ctry <- ".T_FA_F+T_FA_F5A+T_FA_FL.USD_EXC.DI+DO.A+L.ALL+ROU+RSP.D.....A."

# --- Get list of reporting countries ---
if (!file.exists(file.path(data_dir,"fdibuffer","whatctries_fdi.rds"))) {
  cat('Querying reporting country list from OECD FDI...\n')
  whatctries <- try(
    mds(dfprefix %&% 'POS_AGGR/.LE_FA_F.USD_EXC.A.A.ALL.D.....A.',
        labels = FALSE),
    silent = TRUE
  )
  if (is(whatctries, 'md3')) {
    saveRDS(whatctries, file.path(data_dir,"fdibuffer","whatctries_fdi.rds"))
  } else {
    cat('ERROR discovering countries: ', as.character(whatctries), '\n')
    sink()
    stop('Could not discover reporting countries. Check query.')
  }
} else {
  whatctries <- readRDS(file.path(data_dir,"fdibuffer","whatctries_fdi.rds"))
}
ccc <- dimcodes(whatctries)[[1]]
rm(whatctries); gc(full = TRUE, reset = TRUE)

# --- Status tracking ---
statusmat <- matrix(NA_character_,
                    nrow = NROW(ccc), ncol = length(dataflows),
                    dimnames = list(ccc[, 1], names(dataflows)))

dimlog <- vector('list', NROW(ccc) * length(dataflows))
names(dimlog) <- paste(rep(ccc[, 1], each = length(dataflows)),
                       rep(names(dataflows), NROW(ccc)), sep = '_')

# --- Main loading loop ---
cat('\nLoading FDI data for ', NROW(ccc), ' reporting countries x ',
    length(dataflows), ' dataflows\n')

for (cc in ccc[, 1]) {

  cat('\n___ ', as.character(Sys.time()), ': country ',
      match(cc, ccc[, 1]), '/', NROW(ccc), ': ', cc, ' ___\n')

  for (df in names(dataflows)) {
    cachefile <- file.path(data_dir, "fdibuffer", paste0("fdi_",df,"_" ,cc, ".rds"))
    logkey    <- cc %&% '_' %&% df

    cat('  ', df, ': ')

    if (file.exists(cachefile)) {
      # --- Load dims from cache ---
      cat('cache... ')
      temp <- readRDS(cachefile)
      statusmat[cc, df] <- 'L'
      dimlog[[logkey]]  <- dim(temp)
      cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')
      rm(temp)

    } else {
      # --- Query OECD ---
      cat('querying OECD... ')
      gc(full = TRUE, reset = TRUE)

      vdimsel <- switch(df,
        pos_aggr  = vdimsel_pos_aggr,
        pos_ctry  = vdimsel_pos_ctry,
        flow_aggr = vdimsel_flow_aggr,
        flow_ctry = vdimsel_flow_ctry
      )

      qry <- dfprefix %&% dataflows[df] %&% '/' %&% cc %&% vdimsel

      temp <- try(
        mds(qry, drop = FALSE, labels = FALSE),
        silent = TRUE
      )

      if (is(temp, 'md3')) {
        statusmat[cc, df] <- 'I'
        dimlog[[logkey]]  <- dim(temp)
        saveRDS(temp, cachefile)
        cat('OK (', paste(dim(temp), collapse = ' x '), ')\n')

      } else if (any(grepl('err', class(temp)))) {
        msg <- as.character(temp)
        if (any(grepl('0 time series|No results|404', msg, ignore.case = TRUE))) {
          statusmat[cc, df] <- '0'
          cat('no data\n')
        } else {
          statusmat[cc, df] <- 'X'
          cat('ERROR: ', substr(msg[1], 1, 120), '\n')
          Sys.sleep(3)
        }
      }

      rm(temp)
      gc(full = TRUE, reset = TRUE)
    }
  }
}

# --- Reporting ---
cat('\n\n', as.character(Sys.time()), 'FDI loading results\n')
cat('  I = loaded from OECD\n')
cat('  L = loaded from local cache\n')
cat('  0 = no series available from OECD\n')
cat('  X = server loading error\n\n')

print(statusmat)
saveRDS(statusmat, file.path(data_dir,"fdibuffer","statusmat.rds"))

# --- Summary per dataflow ---
cat('\nSummary per dataflow:\n')
for (df in names(dataflows)) {
  nI <- sum(statusmat[, df] == 'I', na.rm = TRUE)
  nL <- sum(statusmat[, df] == 'L', na.rm = TRUE)
  n0 <- sum(statusmat[, df] == '0', na.rm = TRUE)
  nX <- sum(statusmat[, df] == 'X', na.rm = TRUE)
  cat('  ', df, ': loaded ', nI, ' from OECD, ', nL, ' from cache, ',
      n0, ' empty, ', nX, ' errors\n')
}

# --- Failed countries ---
nX_total <- sum(statusmat == 'X', na.rm = TRUE)
if (nX_total > 0) {
  cat('\nFailed queries:\n')
  fails <- which(statusmat == 'X', arr.ind = TRUE)
  for (i in seq_len(nrow(fails))) {
    cat('  ', rownames(statusmat)[fails[i, 1]], ' - ',
        colnames(statusmat)[fails[i, 2]], '\n')
  }
  cat('To retry, delete their cache files and re-run this script.\n')
}

sink()

# --- Build combined results per dataflow from cache ---
cat('Building combined results from cache files...\n')
gc(full = TRUE, reset = TRUE)

for (df in names(dataflows)) {
  pattern <- '^fdi_' %&% df %&% '_.*\\.rds$'
  cachefiles <- dir(file.path(data_dir,"fdibuffer"), pattern = pattern, full.names = TRUE)

  if (length(cachefiles) > 0) {
    lfdi <- vector('list', length(cachefiles))
    names(lfdi) <- gsub('^fdi_' %&% df %&% '_|\\.rds$', '', basename(cachefiles))
    for (i in seq_along(cachefiles)) {
      lfdi[[i]] <- readRDS(cachefiles[i])
    }
    saveRDS(lfdi, file.path(data_dir,"fdibuffer",paste0("allfdi_",df,".rds")))
    cat('  ', df, ': ', length(lfdi), ' countries combined\n')
    rm(lfdi); gc(full = TRUE, reset = TRUE)
  } else {
    cat('  ', df, ': no cache files found\n')
  }
}

cat('Done. Combined results saved to data/loaded/fdibuffer/allfdi_*.rds\n')
