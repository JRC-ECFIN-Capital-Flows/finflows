# 016_filler_bop.R
# Fills aa (assets) and ll (liabilities) with IMF BOP financial account
# transaction data (STO='F') for all reporting countries.
# Finflows 3.0
# Runs BEFORE 017_filler_imf_iip.R.
# -----------------------------------------------------------
# FIXED FINFLOWS COORDINATES FOR ALL FILLS:
#   STO                = F   (flows/transactions)
#   COUNTERPART_AREA   = W0  (world total, non-bilateral source)
#   COUNTERPART_SECTOR = S1  (total economy)
#
# ACCOUNTING ENTRIES:
#   A_NFA_T (net acquisition of financial assets)  -> aa
#   L_NIL_T (net incurrence of liabilities)        -> ll
#
# FREQUENCY HANDLING:
#   The cache holds annual ("2004") and quarterly ("2004q4") TIME
#   codes in the same dimension. ANNUAL OBSERVATIONS ARE DROPPED.
#   A BOP figure is a FLOW: the annual value is the sum of the four
#   quarterly flows, so it can be neither written to q4 (which would
#   put a year of transactions into one quarter) nor spread across
#   quarters (which would invent an unobserved intra-year profile).
#   Finflows is quarterly; annual-only reporters contribute nothing.
#   This is the opposite treatment from 017_filler_imf_iip.R, where
#   an annual figure is an end-of-year STOCK and is relabelled to q4.
#
# CURRENCY:
#   Source is raw USD. Converted to MEUR by direct MD3 division by
#   the quarterly USD/EUR rate, then /1e6.
#
# RESERVE ASSETS:
#   The BOP cache contains NO R_* indicator codes, because the loader
#   requests only the A_NFA_T and L_NIL_T accounting entries. There
#   are therefore no reserve-asset FLOWS in Finflows from this source
#   (reserve POSITIONS do come from IMF IIP). The R_F2/R_F3/R_F5 lines
#   present in the previous version of this script never matched any
#   data and have been removed.
# -----------------------------------------------------------

library(MDstats); library(MD3); library(data.table)

`%&%` = function (..., collapse = NULL, recycle0 = FALSE)  .Internal(paste0(list(...), collapse, recycle0))

# --- Directories ---
if (!exists("data_dir"))   data_dir   = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/filled'
if (!exists("loaded_dir")) loaded_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/loaded'

source('V:/FinFlows/githubrepo/finflows/routines/utilities.R')
gc()

# --- Load aa, ll ---
aa <- readRDS(file.path(data_dir, 'aa_iip_agg.rds')); gc()
ll <- readRDS(file.path(data_dir, 'll_iip_agg.rds')); gc()

# --- Exchange rate: quarterly USD/EUR (MD3 object for direct division) ---
exr <- mds('ECB/EXR/Q.USD.EUR.SP00.E')

# --- Time floor ---
TIME_FLOOR <- '1999q4'

# --- BOP reporter list ---
whatctries <- readRDS(file.path(loaded_dir, 'imfbopbuffer/whatctries_bop.rds'))
ccc  <- dimcodes(whatctries)[[1]]
AREA <- ccc[, 1]
rm(whatctries); gc()

# --- Reporters with no Finflows REF_AREA: skipped ---
#   G309 = Eastern Caribbean Currency Union (no Finflows area code)
SKIP_AREA <- c('G309')
AREA <- setdiff(AREA, SKIP_AREA)

# ===========================================================
# INDICATOR MAPPING TABLE
# -----------------------------------------------------------
# Built code by code against the official IMF labels and against the
# indicator codes actually present in the BOP cache; NOT parsed
# mechanically. ENTRY = NA means the code is mapped under both
# accounting entries; a non-NA ENTRY restricts the code to one side.
#
# EXPLICITLY SKIPPED (no Finflows target, or would double-count):
#   D1_*, D2_*, D3_*, U1_*, U2_*, U3_*  directional-principle FDI
#   *_AFR                               adjusted using IMF accounting records
#   *FEF*                               exceptional-financing memo items
#   *XEF*  (DXEF, OXEF, PXEF, ...)      "excluding exceptional financing"
#                                       variants of totals already mapped
#                                       from their plain codes
#   D_FL                                DI debt instruments: cannot be split
#   D_F5A, D_F5B                        DI equity other than / reinvestment
#                                       of earnings
#   D_F52B, D_F52B_S123, P_F52B         reinvestment of earnings on fund shares
#   O_FL1, O_FL1_*                      "other investment, debt instruments":
#                                       an aggregate with no Finflows INSTR
#   O_TLIMFOTR_*                        credit and loans with the IMF
#   O_F221_S122                         inter-bank positions (not ESA F22)
#   FXR_F712, FXR_F71OTR, FXR_F72,
#   FXR_OPT                             derivative components: no F71/F72 target
#   maturity splits (_L/_S) on          no F2L/F2S, F81L/F81S, F89L/F89S
#   currency & deposits, trade          in the aa INSTR codelist
#   credits, other accounts
# ===========================================================

bop_mapping <- data.table(
  BOP_INDICATOR  = character(),
  INSTR          = character(),
  FUNCTIONAL_CAT = character(),
  REF_SECTOR     = character(),
  ENTRY          = character()
)

add_map <- function(ind, instr, funccat, refsect, entry = NA_character_) {
  bop_mapping <<- rbind(bop_mapping,
                        data.table(BOP_INDICATOR = ind, INSTR = instr,
                                   FUNCTIONAL_CAT = funccat, REF_SECTOR = refsect,
                                   ENTRY = entry))
}

# The seven sector suffixes present in BOP and in the aa REF_SECTOR codelist
SS7 <- c('S121', 'S122', 'S12R', 'S13', 'S1V', 'S1X', 'S1Z')

# ===== DIRECT INVESTMENT (_D) =====
add_map('D_F',  'F',  '_D', 'S1')
add_map('D_F3', 'F3', '_D', 'S1')
add_map('D_F5', 'F5', '_D', 'S1')

# ===== PORTFOLIO INVESTMENT (_P) =====
add_map('P_F',          'F',    '_P', 'S1')
add_map('P_F3',         'F3',   '_P', 'S1')
add_map('P_F5',         'F5',   '_P', 'S1')
add_map('P_F51',        'F51',  '_P', 'S1')
add_map('P_F511',       'F511', '_P', 'S1')
add_map('P_F512',       'F512', '_P', 'S1')
add_map('P_F52',        'F52',  '_P', 'S1')
add_map('P_F52_S123',   'F52',  '_P', 'S123')   # S123 = holding sector (money market funds)

for (ss in SS7) {
  add_map('P_F3_' %&% ss,          'F3',  '_P', ss)
  add_map('P_F3_' %&% ss %&% '_L', 'F3L', '_P', ss)
  add_map('P_F3_' %&% ss %&% '_S', 'F3S', '_P', ss)
  add_map('P_F5_' %&% ss,          'F5',  '_P', ss)
}

# ===== OTHER INVESTMENT (_O) =====
add_map('O_F',        'F',    '_O', 'S1')
add_map('O_F12_S1N',  'F12',  '_O', 'S1')   # SDRs (published "not sectorised")
add_map('O_F2',       'F2',   '_O', 'S1')
add_map('O_F4',       'F4',   '_O', 'S1')
add_map('O_F519',     'F519', '_O', 'S1')   # other equity
add_map('O_F6',       'F6',   '_O', 'S1')
add_map('O_F61',      'F61',  '_O', 'S1')
add_map('O_F62',      'F62',  '_O', 'S1')
add_map('O_F63',      'F63',  '_O', 'S1')
add_map('O_F64',      'F64',  '_O', 'S1')
add_map('O_F65',      'F65',  '_O', 'S1')
add_map('O_F66',      'F66',  '_O', 'S1')
add_map('O_F81',      'F81',  '_O', 'S1')

for (ss in SS7) {
  add_map('O_F2_'  %&% ss,          'F2',  '_O', ss)
  add_map('O_F4_'  %&% ss,          'F4',  '_O', ss)
  add_map('O_F4_'  %&% ss %&% '_L', 'F4L', '_O', ss)
  add_map('O_F4_'  %&% ss %&% '_S', 'F4S', '_O', ss)
  add_map('O_F6_'  %&% ss,          'F6',  '_O', ss)
  add_map('O_F81_' %&% ss,          'F81', '_O', ss)
}

# Other accounts receivable/payable -> F89, restricted by accounting entry:
#   acquisition of financial assets is a receivable;
#   incurrence of liabilities is a payable.
add_map('O_F8AR', 'F89', '_O', 'S1', 'A_NFA_T')
add_map('O_F8AP', 'F89', '_O', 'S1', 'L_NIL_T')
for (ss in SS7) {
  add_map('O_F8AR_' %&% ss, 'F89', '_O', ss, 'A_NFA_T')
  add_map('O_F8AP_' %&% ss, 'F89', '_O', ss, 'L_NIL_T')
}

# ===== FINANCIAL DERIVATIVES (_F) =====
# _F is the BPM6 functional category "financial derivatives", not a
# flows marker (flows/stocks is the STO dimension). INSTR is F7.
add_map('F_F7', 'F7', '_F', 'S1')
for (ss in SS7) {
  add_map('F_F7_' %&% ss, 'F7', '_F', ss)
}

# --- Expand ENTRY = NA into both accounting entries ---
map_both <- bop_mapping[is.na(ENTRY)]
map_one  <- bop_mapping[!is.na(ENTRY)]
bop_mapping <- rbindlist(list(
  copy(map_both)[, ENTRY := 'A_NFA_T'],
  copy(map_both)[, ENTRY := 'L_NIL_T'],
  map_one
))
rm(map_both, map_one)

setnames(bop_mapping, 'ENTRY', 'BOP_ACCOUNTING_ENTRY')

# --- Integrity check: the mapping must be injective within an entry,
#     otherwise two indicators would compete for the same Finflows cell ---
dupes <- bop_mapping[, .N,
                     by = .(BOP_ACCOUNTING_ENTRY, INSTR, FUNCTIONAL_CAT, REF_SECTOR)][N > 1]
if (nrow(dupes) > 0) {
  print(dupes)
  stop('Mapping table is not injective: two indicators target the same Finflows cell.')
}

cat('BOP mapping table: ', nrow(bop_mapping), ' indicator-entry rows (',
    uniqueN(bop_mapping$BOP_INDICATOR), ' distinct indicators)\n')

# ===========================================================
# HELPER
# ===========================================================

# Fill aa and ll for one country from an already-mapped data.table
fill_aa_ll_bop <- function(dt_mapped, cc2) {
  
  # ---- FILL AA (assets) from A_NFA_T ----
  dt_a <- dt_mapped[BOP_ACCOUNTING_ENTRY == 'A_NFA_T',
                    .(INSTR, REF_SECTOR, FUNCTIONAL_CAT, TIME, obs_value)]
  
  if (nrow(dt_a) > 0) {
    dim_struct_a <- lapply(
      c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT'),
      function(d) sort(unique(dt_a[[d]]))
    )
    names(dim_struct_a) <- c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT')
    
    bop_a_md3 <- as.md3(
      dt_a,
      id.vars  = c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT', 'TIME'),
      dcstruct = dim_struct_a,
      timeid   = 'TIME',
      na.rm    = TRUE
    )
    
    # USD -> MEUR (direct MD3 division by the quarterly USD/EUR rate)
    bop_a_md3 <- bop_a_md3 / exr
    bop_a_md3 <- bop_a_md3 / 1e6
    bop_a_md3 <- round(bop_a_md3, 2)
    
    # aa dims: INSTR(1), REF_AREA(2), REF_SECTOR(3), COUNTERPART_SECTOR(4),
    #          STO(5), FUNCTIONAL_CAT(6), TIME(7), COUNTERPART_AREA(8)
    aa[, cc2, , 'S1', 'F', , , 'W0', usenames = TRUE, onlyna = TRUE] <<- bop_a_md3
    
    rm(bop_a_md3)
  }
  
  # ---- FILL LL (liabilities) from L_NIL_T ----
  dt_l <- dt_mapped[BOP_ACCOUNTING_ENTRY == 'L_NIL_T',
                    .(INSTR, REF_SECTOR, FUNCTIONAL_CAT, TIME, obs_value)]
  
  if (nrow(dt_l) > 0) {
    dim_struct_l <- lapply(
      c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT'),
      function(d) sort(unique(dt_l[[d]]))
    )
    names(dim_struct_l) <- c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT')
    
    bop_l_md3 <- as.md3(
      dt_l,
      id.vars  = c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT', 'TIME'),
      dcstruct = dim_struct_l,
      timeid   = 'TIME',
      na.rm    = TRUE
    )
    
    bop_l_md3 <- bop_l_md3 / exr
    bop_l_md3 <- bop_l_md3 / 1e6
    bop_l_md3 <- round(bop_l_md3, 2)
    
    # ll dims: INSTR(1), COUNTERPART_AREA(2), COUNTERPART_SECTOR(3),
    #          REF_SECTOR(4), STO(5), FUNCTIONAL_CAT(6), TIME(7), REF_AREA(8)
    ll[, 'W0', 'S1', , 'F', , , cc2, usenames = TRUE, onlyna = TRUE] <<- bop_l_md3
    
    rm(bop_l_md3)
  }
}

# ===========================================================
# MAIN LOOP
# ===========================================================

cat('\n=== Processing IMF BOP data for', length(AREA), 'countries ===\n')

for (cc in AREA) {
  
  cachefile <- file.path(loaded_dir, 'imfbopbuffer/bop_' %&% cc %&% '.rds')
  if (!file.exists(cachefile)) { cat(cc, ': no cache file, skipping\n'); next }
  
  cat(as.character(Sys.time()), ': ', match(cc, AREA), '/', length(AREA),
      ' ', cc, '... ')
  
  bop_cc <- readRDS(cachefile)
  
  # --- Country code conversion ---
  cc2 <- ccode(cc, 'iso3c', 'iso2m', leaveifNA = TRUE, warn = FALSE)
  if (cc2 == 'CN')   cc2 <- 'CN_X_HK'
  if (cc2 == 'G163') cc2 <- 'EA20'   # Euro Area
  if (cc2 == 'WBG')  cc2 <- 'PS'     # West Bank and Gaza
  # CWX (Curacao and Sint Maarten) is already a valid REF_AREA code
  
  # --- To data.table (raw USD, mixed annual/quarterly TIME) ---
  dt <- as.data.table(bop_cc, .simple = TRUE, na.rm = TRUE)
  rm(bop_cc)
  
  if (nrow(dt) == 0) { cat('empty cache\n'); gc(); next }
  
  # The value column name starts with an underscore
  VAL <- setdiff(names(dt),
                 c('COUNTRY', 'BOP_ACCOUNTING_ENTRY', 'INDICATOR', 'UNIT', 'TIME'))
  setnames(dt, VAL, 'obs_value')
  
  for (col in c('COUNTRY', 'UNIT')) {
    if (col %in% names(dt)) dt[, (col) := NULL]
  }
  
  dt <- dt[BOP_ACCOUNTING_ENTRY %in% c('A_NFA_T', 'L_NIL_T')]
  if (nrow(dt) == 0) { cat('no A_NFA_T/L_NIL_T data\n'); rm(dt); gc(); next }
  
  # --- TIME: quarterly only, annual dropped, floor applied ---
  dt <- dt[grepl('q', TIME, fixed = TRUE) & TIME >= TIME_FLOOR]
  if (nrow(dt) == 0) { cat('no quarterly data at or after ', TIME_FLOOR, '\n'); rm(dt); gc(); next }
  
  # --- Merge with mapping table (inner join on indicator AND entry) ---
  dt_mapped <- merge(dt, bop_mapping,
                     by.x = c('INDICATOR', 'BOP_ACCOUNTING_ENTRY'),
                     by.y = c('BOP_INDICATOR', 'BOP_ACCOUNTING_ENTRY'))
  rm(dt)
  
  if (nrow(dt_mapped) == 0) { cat('no mapped indicators\n'); gc(); next }
  
  fill_aa_ll_bop(dt_mapped, cc2)
  
  rm(dt_mapped)
  gc()
  cat('done\n')
}

# ===========================================================
# SAVE
# ===========================================================

saveRDS(aa, file.path(data_dir, 'aa_iip_imf_bop.rds'))
saveRDS(ll, file.path(data_dir, 'll_iip_imf_bop.rds'))

saveRDS(aa, file.path(data_dir, 'vintages/aa_iip_imf_bop_' %&% format(Sys.time(), '%F') %&% '_.rds'))
saveRDS(ll, file.path(data_dir, 'vintages/ll_iip_imf_bop_' %&% format(Sys.time(), '%F') %&% '_.rds'))

cat('Done. Saved aa_iip_imf_bop.rds and ll_iip_imf_bop.rds\n')