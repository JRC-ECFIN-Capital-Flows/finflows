# 017_filler_imf_iip.R
# Fills aa (assets) and ll (liabilities) with IMF IIP international
# investment position data (STO='LE') for all reporting countries.
# Finflows 3.0
# Runs AFTER 016_filler_bop.R.
# -----------------------------------------------------------
# FIXED FINFLOWS COORDINATES FOR ALL FILLS:
#   STO                = LE  (positions/stocks)
#   COUNTERPART_AREA   = W0  (world total, non-bilateral source)
#   COUNTERPART_SECTOR = S1  (total economy)
#
# ACCOUNTING ENTRIES:
#   A_P (assets, positions)      -> aa
#   L_P (liabilities, positions) -> ll
#
# FREQUENCY HANDLING:
#   The cache holds annual ("2004") and quarterly ("2004q4") TIME
#   codes in the same dimension. An annual IIP figure is an
#   END-OF-YEAR STOCK, so it is relabelled to q4 of its reference
#   year and NOT spread over the four quarters. Where a quarterly
#   observation and a relabelled annual observation coincide on the
#   same cell, the quarterly one wins (PRIORITY column below).
#   All fills are capped at TIME >= 1999q4.
#
# CURRENCY:
#   Source is raw USD. Converted to MEUR by direct MD3 division by
#   the quarterly USD/EUR rate, then /1e6. Because annual figures
#   are end-of-year stocks relabelled to q4, they are converted at
#   the Q4 rate of the year - the correct end-of-period rate.
# -----------------------------------------------------------

library(MDstats); library(MD3); library(data.table)

`%&%` = function (..., collapse = NULL, recycle0 = FALSE)  .Internal(paste0(list(...), collapse, recycle0))

# --- Directories (new server; UNC path hardcoded for portability across machines) ---
data_dir   = '\\\\siprsto06p.delta.europa.eu/ECOFIN/FinFlows/githubrepo/data/filled'
loaded_dir = '\\\\siprsto06p.delta.europa.eu/ECOFIN/FinFlows/githubrepo/data/loaded'

source('\\\\siprsto06p.delta.europa.eu/ECOFIN/FinFlows/githubrepo/finflows/routines/utilities.R')
gc()

# --- Load aa, ll (outputs of 016_filler_bop.R) ---
aa <- readRDS(file.path(data_dir, 'aa_iip_imf_bop.rds')); gc()
ll <- readRDS(file.path(data_dir, 'll_iip_imf_bop.rds')); gc()

# --- Exchange rate: quarterly USD/EUR (MD3 object for direct division) ---
exr <- mds('ECB/EXR/Q.USD.EUR.SP00.E')

# --- Time floor ---
TIME_FLOOR <- '1999q4'

# --- IIP reporter list ---
whatctries <- readRDS(file.path(loaded_dir, 'imfiipbuffer/whatctries_iip.rds'))
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
# Built code by code against the official IMF labels; NOT parsed
# mechanically. ENTRY = NA means the code is mapped under both
# A_P and L_P; a non-NA ENTRY restricts the code to one side.
# ===========================================================

iip_mapping <- data.table(
  IIP_INDICATOR  = character(),
  INSTR          = character(),
  FUNCTIONAL_CAT = character(),
  REF_SECTOR     = character(),
  ENTRY          = character()
)

add_map <- function(ind, instr, funccat, refsect, entry = NA_character_) {
  iip_mapping <<- rbind(iip_mapping,
                        data.table(IIP_INDICATOR = ind, INSTR = instr,
                                   FUNCTIONAL_CAT = funccat, REF_SECTOR = refsect,
                                   ENTRY = entry))
}

# The seven sector suffixes present in IIP and in the aa REF_SECTOR codelist
SS7 <- c('S121', 'S122', 'S12R', 'S13', 'S1V', 'S1X', 'S1Z')

# ===== TOTAL (_T) =====
add_map('IIP', 'F', '_T', 'S1')

# ===== DIRECT INVESTMENT (_D) =====
add_map('D',             'F',   '_D', 'S1')
add_map('D_F3',          'F3',  '_D', 'S1')
add_map('D_F5',          'F5',  '_D', 'S1')
add_map('D_F52_MV',      'F52', '_D', 'S1')
add_map('D_F52_S123_MV', 'F52', '_D', 'S123')   # S123 = holding sector (money market funds)

# ===== PORTFOLIO INVESTMENT (_P) =====
add_map('P_MV',          'F',    '_P', 'S1')
add_map('P_F3_MV',       'F3',   '_P', 'S1')
add_map('P_F5_MV',       'F5',   '_P', 'S1')
add_map('P_F51_MV',      'F51',  '_P', 'S1')
add_map('P_F511_MV',     'F511', '_P', 'S1')
add_map('P_F512_MV',     'F512', '_P', 'S1')
add_map('P_F52_MV',      'F52',  '_P', 'S1')
add_map('P_F52_S123_MV', 'F52',  '_P', 'S123')  # S123 = holding sector (money market funds)

for (ss in SS7) {
  add_map('P_F3_' %&% ss %&% '_MV',   'F3',  '_P', ss)
  add_map('P_F3_' %&% ss %&% '_L_MV', 'F3L', '_P', ss)
  add_map('P_F3_' %&% ss %&% '_S_MV', 'F3S', '_P', ss)
  add_map('P_F5_' %&% ss %&% '_MV',   'F5',  '_P', ss)
}

# ===== OTHER INVESTMENT (_O) =====
add_map('O',         'F',    '_O', 'S1')
add_map('O_F12',     'F12',  '_O', 'S1')   # SDRs
add_map('O_F2_NV',   'F2',   '_O', 'S1')
add_map('O_F4_NV',   'F4',   '_O', 'S1')
add_map('O_F519_MV', 'F519', '_O', 'S1')   # other equity
add_map('O_F6',      'F6',   '_O', 'S1')
add_map('O_F61',     'F61',  '_O', 'S1')
add_map('O_F62',     'F62',  '_O', 'S1')
add_map('O_F63',     'F63',  '_O', 'S1')
add_map('O_F64',     'F64',  '_O', 'S1')
add_map('O_F65',     'F65',  '_O', 'S1')
add_map('O_F66',     'F66',  '_O', 'S1')
add_map('O_F81',     'F81',  '_O', 'S1')

for (ss in SS7) {
  add_map('O_F2_' %&% ss %&% '_NV',   'F2',  '_O', ss)
  add_map('O_F4_' %&% ss %&% '_NV',   'F4',  '_O', ss)
  add_map('O_F4_' %&% ss %&% '_L_NV', 'F4L', '_O', ss)
  add_map('O_F4_' %&% ss %&% '_S_NV', 'F4S', '_O', ss)
  add_map('O_F6_' %&% ss,             'F6',  '_O', ss)
  add_map('O_F81_' %&% ss,            'F81', '_O', ss)
}

# Other accounts receivable/payable -> F89, restricted by accounting entry:
#   an asset position is a receivable; a liability position is a payable.
add_map('O_F8AR', 'F89', '_O', 'S1', 'A_P')
add_map('O_F8AP', 'F89', '_O', 'S1', 'L_P')
for (ss in SS7) {
  add_map('O_F8AR_' %&% ss, 'F89', '_O', ss, 'A_P')
  add_map('O_F8AP_' %&% ss, 'F89', '_O', ss, 'L_P')
}

# ===== RESERVES (_R) =====
add_map('R',         'F',   '_R', 'S1')
add_map('R_F11_MV',  'F11', '_R', 'S1')   # monetary gold
add_map('R_F12_MV',  'F12', '_R', 'S1')   # SDRs
add_map('R_F2_NV',   'F2',  '_R', 'S1')
add_map('R_F3_MV',   'F3',  '_R', 'S1')
add_map('R_F3_L_MV', 'F3L', '_R', 'S1')
add_map('R_F3_S_MV', 'F3S', '_R', 'S1')
add_map('R_F5_MV',   'F5',  '_R', 'S1')
add_map('R_F71_T',   'F7',  '_R', 'S1')

# ===== FINANCIAL DERIVATIVES (_F) =====
add_map('F_F7_T', 'F7', '_F', 'S1')
for (ss in SS7) {
  add_map('F_F7_' %&% ss %&% '_T', 'F7', '_F', ss)
}

# --- Expand ENTRY = NA into both accounting entries ---
map_both <- iip_mapping[is.na(ENTRY)]
map_one  <- iip_mapping[!is.na(ENTRY)]
iip_mapping <- rbindlist(list(
  copy(map_both)[, ENTRY := 'A_P'],
  copy(map_both)[, ENTRY := 'L_P'],
  map_one
))
rm(map_both, map_one)

setnames(iip_mapping, 'ENTRY', 'BOP_ACCOUNTING_ENTRY')

# --- Integrity check: the mapping must be injective within an entry,
#     otherwise two indicators would compete for the same Finflows cell ---
dupes <- iip_mapping[, .N,
                     by = .(BOP_ACCOUNTING_ENTRY, INSTR, FUNCTIONAL_CAT, REF_SECTOR)][N > 1]
if (nrow(dupes) > 0) {
  print(dupes)
  stop('Mapping table is not injective: two indicators target the same Finflows cell.')
}

cat('IIP mapping table: ', nrow(iip_mapping), ' indicator-entry rows (',
    uniqueN(iip_mapping$IIP_INDICATOR), ' distinct indicators)\n')

# ===========================================================
# HELPERS
# ===========================================================

# Relabel annual TIME codes to q4 of the reference year.
# Quarterly codes (containing 'q') pass through unchanged.
# PRIORITY: 1 = genuinely quarterly, 2 = annual relabelled to q4.
make_time_q <- function(tt) {
  tt  <- as.character(tt)
  isq <- grepl('q', tt, fixed = TRUE)
  data.table(
    TIME     = tt,
    TIME_Q   = fifelse(isq, tt, tt %&% 'q4'),
    PRIORITY = fifelse(isq, 1L, 2L)
  )
}

# Fill aa and ll for one country from an already-mapped, deduplicated data.table
fill_aa_ll_iip <- function(dt_mapped, cc2) {
  
  # ---- FILL AA (assets) from A_P ----
  dt_a <- dt_mapped[BOP_ACCOUNTING_ENTRY == 'A_P',
                    .(INSTR, REF_SECTOR, FUNCTIONAL_CAT, TIME, obs_value)]
  
  if (nrow(dt_a) > 0) {
    dim_struct_a <- lapply(
      c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT'),
      function(d) sort(unique(dt_a[[d]]))
    )
    names(dim_struct_a) <- c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT')
    
    iip_a_md3 <- as.md3(
      dt_a,
      id.vars  = c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT', 'TIME'),
      dcstruct = dim_struct_a,
      timeid   = 'TIME',
      na.rm    = TRUE
    )
    
    # USD -> MEUR (direct MD3 division by the quarterly USD/EUR rate)
    iip_a_md3 <- iip_a_md3 / exr
    iip_a_md3 <- iip_a_md3 / 1e6
    iip_a_md3 <- round(iip_a_md3, 2)
    
    # aa dims: INSTR(1), REF_AREA(2), REF_SECTOR(3), COUNTERPART_SECTOR(4),
    #          STO(5), FUNCTIONAL_CAT(6), TIME(7), COUNTERPART_AREA(8)
    aa[, cc2, , 'S1', 'LE', , , 'W0', usenames = TRUE, onlyna = TRUE] <<- iip_a_md3
    
    rm(iip_a_md3)
  }
  
  # ---- FILL LL (liabilities) from L_P ----
  dt_l <- dt_mapped[BOP_ACCOUNTING_ENTRY == 'L_P',
                    .(INSTR, REF_SECTOR, FUNCTIONAL_CAT, TIME, obs_value)]
  
  if (nrow(dt_l) > 0) {
    dim_struct_l <- lapply(
      c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT'),
      function(d) sort(unique(dt_l[[d]]))
    )
    names(dim_struct_l) <- c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT')
    
    iip_l_md3 <- as.md3(
      dt_l,
      id.vars  = c('INSTR', 'REF_SECTOR', 'FUNCTIONAL_CAT', 'TIME'),
      dcstruct = dim_struct_l,
      timeid   = 'TIME',
      na.rm    = TRUE
    )
    
    iip_l_md3 <- iip_l_md3 / exr
    iip_l_md3 <- iip_l_md3 / 1e6
    iip_l_md3 <- round(iip_l_md3, 2)
    
    # ll dims: INSTR(1), COUNTERPART_AREA(2), COUNTERPART_SECTOR(3),
    #          REF_SECTOR(4), STO(5), FUNCTIONAL_CAT(6), TIME(7), REF_AREA(8)
    ll[, 'W0', 'S1', , 'LE', , , cc2, usenames = TRUE, onlyna = TRUE] <<- iip_l_md3
    
    rm(iip_l_md3)
  }
}

# ===========================================================
# MAIN LOOP
# ===========================================================

cat('\n=== Processing IMF IIP data for', length(AREA), 'countries ===\n')

for (cc in AREA) {
  
  cachefile <- file.path(loaded_dir, 'imfiipbuffer/iip_' %&% cc %&% '.rds')
  if (!file.exists(cachefile)) { cat(cc, ': no cache file, skipping\n'); next }
  
  cat(as.character(Sys.time()), ': ', match(cc, AREA), '/', length(AREA),
      ' ', cc, '... ')
  
  iip_cc <- readRDS(cachefile)
  
  # --- Country code conversion ---
  cc2 <- ccode(cc, 'iso3c', 'iso2m', leaveifNA = TRUE, warn = FALSE)
  if (cc2 == 'CN')   cc2 <- 'CN_X_HK'
  if (cc2 == 'G163') cc2 <- 'EA20'   # Euro Area
  if (cc2 == 'WBG')  cc2 <- 'PS'     # West Bank and Gaza
  # CWX (Curacao and Sint Maarten) is already a valid REF_AREA code
  
  # --- To data.table (raw USD, mixed annual/quarterly TIME) ---
  dt <- as.data.table(iip_cc, .simple = TRUE, na.rm = TRUE)
  rm(iip_cc)
  
  if (nrow(dt) == 0) { cat('empty cache\n'); gc(); next }
  
  # TIME arrives as integer64 (mixed annual/quarterly); force to character
  dt[, TIME := as.character(TIME)]
  
  # The value column name starts with an underscore
  VAL <- setdiff(names(dt),
                 c('COUNTRY', 'BOP_ACCOUNTING_ENTRY', 'INDICATOR', 'UNIT', 'TIME'))
  setnames(dt, VAL, 'obs_value')
  
  for (col in c('COUNTRY', 'UNIT')) {
    if (col %in% names(dt)) dt[, (col) := NULL]
  }
  
  dt <- dt[BOP_ACCOUNTING_ENTRY %in% c('A_P', 'L_P')]
  if (nrow(dt) == 0) { cat('no A_P/L_P data\n'); rm(dt); gc(); next }
  
  # --- Merge with mapping table (inner join on indicator AND entry) ---
  dt_mapped <- merge(dt, iip_mapping,
                     by.x = c('INDICATOR', 'BOP_ACCOUNTING_ENTRY'),
                     by.y = c('IIP_INDICATOR', 'BOP_ACCOUNTING_ENTRY'))
  rm(dt)
  
  if (nrow(dt_mapped) == 0) { cat('no mapped indicators\n'); gc(); next }
  
  # --- TIME: relabel annual to q4, tag priority, apply floor ---
  tmap <- make_time_q(unique(dt_mapped$TIME))
  dt_mapped <- merge(dt_mapped, tmap, by = 'TIME')
  rm(tmap)
  
  dt_mapped <- dt_mapped[TIME_Q >= TIME_FLOOR]
  if (nrow(dt_mapped) == 0) { cat('nothing at or after ', TIME_FLOOR, '\n'); gc(); next }
  
  dt_mapped[, TIME := TIME_Q]
  dt_mapped[, TIME_Q := NULL]
  
  # --- Quarterly beats annual on any cell where both exist ---
  setorder(dt_mapped, BOP_ACCOUNTING_ENTRY, INSTR, REF_SECTOR, FUNCTIONAL_CAT,
           TIME, PRIORITY)
  dt_mapped <- unique(dt_mapped,
                      by = c('BOP_ACCOUNTING_ENTRY', 'INSTR', 'REF_SECTOR',
                             'FUNCTIONAL_CAT', 'TIME'))
  dt_mapped[, PRIORITY := NULL]
  
  fill_aa_ll_iip(dt_mapped, cc2)
  
  rm(dt_mapped)
  gc()
  cat('done\n')
}

# ===========================================================
# SAVE
# ===========================================================

saveRDS(aa, file.path(data_dir, 'aa_iip_imf.rds'))
saveRDS(ll, file.path(data_dir, 'll_iip_imf.rds'))

saveRDS(aa, file.path(data_dir, 'vintages/aa_iip_imf_' %&% format(Sys.time(), '%F') %&% '_.rds'))
saveRDS(ll, file.path(data_dir, 'vintages/ll_iip_imf_' %&% format(Sys.time(), '%F') %&% '_.rds'))

cat('Done. Saved aa_iip_imf.rds and ll_iip_imf.rds\n')