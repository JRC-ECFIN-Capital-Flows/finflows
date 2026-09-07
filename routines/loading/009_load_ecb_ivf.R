# ==============================================================================
# 008_z_load_ecb_ivf.R
# Load ECB Investment Fund Statistics (IVF) into Finflows pipeline
# Reference sector: S124 (investment funds)
#
# Asset-side items:
#   A20 → F4  (loans)          | maturity-split: A/F/K → F4/F4S/F4L
#   A30 → F3  (debt sec.)      | maturity-split: A/F/K → F3/F3S/F3L
#   A50 → F5  (equity + IF sh.)| maturity A only
#   A5A → F51 (equity)         | maturity A only
#   A51 → F511(listed shares)  | maturity A only
#   A52 → F52 (IF shares)      | maturity A only
#   L20 → F2M (deposits held)  | maturity A only
#
# Liability-side items:
#   L30 → F52 (fund shares issued)
#
# Skipped items: A31 (not populated), A60, AT1, LT1, T00
# ==============================================================================

library(MDstats)
library(MD3)

if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/loaded'

`%&%` = function (..., collapse = NULL, recycle0 = FALSE) .Internal(paste0(list(...), collapse, recycle0))

# ==============================================================================
# 1. DOWNLOAD: quarterly and monthly IVF data
#    Filters: T0 (total original maturity bucket), A+F+K (maturities),
#             1+4 (stocks+flows), Z01.E (EUR, standard)
# ==============================================================================

ivfrawq = mds('ECB/IVF/Q..N.T0..A+F+K.1+4...Z01.E', labels = TRUE,  ccode = NULL)
ivfrawm = mds('ECB/IVF/M..N.T0..A+F+K.1+4...Z01.E', labels = FALSE, ccode = NULL)
gc()

# ==============================================================================
# 2. AGGREGATE monthly to quarterly (end-of-period stocks)
# ==============================================================================

ivfraw = aggregate(ivfrawm, 'Q', FUN = end)
gc()

# ==============================================================================
# 3. MERGE: quarterly observations into monthly-aggregated base
#    (quarterly has richer breakdowns; monthly extends time coverage
#     for coarser breakdowns)
# ==============================================================================

ivfraw = copy(ivfraw)
suppressWarnings(ivfraw[usenames = TRUE] <- ivfrawq)

# Enrich labels from ECB metadata
for (i in setdiff(names(dimnames(ivfraw)), 'TIME')) {
  dimcodes(ivfraw)[[i]][, 'label:en'] = helpmds('ECB/IVF', dim = i, verbose = FALSE)[dimnames(ivfraw)[[i]], 'label:en']
}
gc()

# Save raw merged object
saveRDS(ivfraw, file.path(data_dir, 'ivfraw.rds'))
saveRDS(ivfraw, file.path(data_dir, 'vintages/ivfraw_' %&% format(Sys.time(), '%F') %&% '_.rds'))

# ==============================================================================
# 4. RENAME dimensions to Finflows conventions
#    Original IVF dimension order (7 dims):
#      REF_AREA . IVF_ITEM . MATURITY_ORIG . DATA_TYPE .
#      COUNT_AREA . BS_COUNT_SECTOR . TIME
# ==============================================================================

ivf = copy(ivfraw)
names(dimnames(ivf))[names(dimnames(ivf)) == 'IVF_ITEM']        = 'INSTR'
names(dimnames(ivf))[names(dimnames(ivf)) == 'MATURITY_ORIG']   = 'MATURITY'
names(dimnames(ivf))[names(dimnames(ivf)) == 'DATA_TYPE']       = 'STO'
names(dimnames(ivf))[names(dimnames(ivf)) == 'COUNT_AREA']      = 'COUNTERPART_AREA'
names(dimnames(ivf))[names(dimnames(ivf)) == 'BS_COUNT_SECTOR'] = 'COUNTERPART_SECTOR'

# Dimension order after rename (7 dims, 6 dots):
#   REF_AREA . INSTR . MATURITY . STO . COUNTERPART_AREA . COUNTERPART_SECTOR . TIME

# ==============================================================================
# 5. SECTOR DICTIONARY (BSI numeric codes → ESA2010 sector codes)
# ==============================================================================

dictivfsec = c('0000' = 'S1',   '1000' = 'S12K', '2100' = 'S13',
               '2220' = 'S12Q', '2240' = 'S11',  '2250' = 'S1M',
               '2260' = 'S124', '2270' = 'S12O')

available_secs = intersect(names(dictivfsec), dimnames(ivf)$COUNTERPART_SECTOR)
ivf = ivf[, , , , , available_secs, ]
dimnames(ivf)$COUNTERPART_SECTOR = dictivfsec[dimnames(ivf)$COUNTERPART_SECTOR]

# ==============================================================================
# 6. STO RENAME: 1 → LE (stocks/positions), 4 → F (flows/transactions)
# ==============================================================================

dimnames(ivf)$STO = c('1' = 'LE', '4' = 'F')[dimnames(ivf)$STO]

# ==============================================================================
# 7. COUNTERPART_AREA: drop Z5 (world not allocated) and U8 (not used)
# ==============================================================================

drop_ca = intersect(c('Z5', 'U8'), dimnames(ivf)$COUNTERPART_AREA)
if (length(drop_ca) > 0) {
  ivf = ivf[, , , , setdiff(dimnames(ivf)$COUNTERPART_AREA, drop_ca), , ]
}
gc()

# ==============================================================================
# 8. U2 → TIME-VARYING EA MAPPING
#    U2 = euro area (changing composition). Each observation is mapped to the
#    EA vintage that was current at that time.
#    Applied to both REF_AREA (dim 1) and COUNTERPART_AREA (dim 5).
#
#    EA11: –2000q4          (original 11)
#    EA12: 2001q1–2006q4    (+ GR)
#    EA13: 2007q1–2007q4    (+ SI)
#    EA15: 2008q1–2008q4    (+ CY, MT)
#    EA16: 2009q1–2010q4    (+ SK)
#    EA17: 2011q1–2013q4    (+ EE)
#    EA18: 2014q1–2014q4    (+ LV)
#    EA19: 2015q1–2022q4    (+ LT)
#    EA20: 2023q1–2025q4    (+ HR)
#    EA21: 2026q1–          (+ BG)
# ==============================================================================

ea_vintages = list(
  EA11 = '1999q1:2000q4',
  EA12 = '2001q1:2006q4',
  EA13 = '2007q1:2007q4',
  EA15 = '2008q1:2008q4',
  EA16 = '2009q1:2010q4',
  EA17 = '2011q1:2013q4',
  EA18 = '2014q1:2014q4',
  EA19 = '2015q1:2022q4',
  EA20 = '2023q1:2025q4',
  EA21 = '2026q1:2099q4'
)

# --- REF_AREA (dim 1): U2 → EA vintages ---
if ('U2' %in% dimnames(ivf)$REF_AREA) {
  for (ea in names(ea_vintages)) {
    tr = ea_vintages[[ea]]
    ivf[ea, , , , , , tr, usenames = TRUE] = ivf['U2', , , , , , tr]
  }
  ivf = ivf[setdiff(dimnames(ivf)$REF_AREA, 'U2'), , , , , , ]
  gc()
}

# --- COUNTERPART_AREA (dim 5): U2 → EA vintages ---
if ('U2' %in% dimnames(ivf)$COUNTERPART_AREA) {
  for (ea in names(ea_vintages)) {
    tr = ea_vintages[[ea]]
    ivf[, , , , ea, , tr, usenames = TRUE] = ivf[, , , , 'U2', , tr]
  }
  ivf = ivf[, , , , setdiff(dimnames(ivf)$COUNTERPART_AREA, 'U2'), , ]
  gc()
}

# ==============================================================================
# 9. REMAINING COUNTERPART_AREA CODES (kept as-is)
#    U5 = other EA member states (EA minus reporter) — NOT an EA aggregate
#    U4 = extra euro area
#    U3 = non-EA EU member states (U2 + U3 = EU27, to be computed downstream)
#    U6 = domestic
#    GB, JP, US = individual countries (harmonised by ccode below)
# ==============================================================================

# ==============================================================================
# 10. COUNTRY CODE HARMONISATION (iso2m via ccode)
#     Handles GR→EL, GB→UK, etc.
#     leaveifNA=TRUE keeps EA codes and aggregate codes (U3, U4, U6) as-is.
# ==============================================================================

dimnames(ivf)$REF_AREA        = ccode(dimnames(ivf)$REF_AREA,        2, 'iso2m', leaveifNA = TRUE)
dimnames(ivf)$COUNTERPART_AREA = ccode(dimnames(ivf)$COUNTERPART_AREA, 2, 'iso2m', leaveifNA = TRUE)
gc()

# ==============================================================================
# 11. SPLIT: ASSETS vs LIABILITIES
#     Done before instrument renaming to avoid F52 collision
#     (A52 → F52 on asset side; L30 → F52 on liability side)
# ==============================================================================

# Asset items: A20(F4), A30(F3), A50(F5), A5A(F51), A51(F511), A52(F52), L20(F2M)
asset_items = intersect(c('A20', 'A30', 'A50', 'A5A', 'A51', 'A52', 'L20'),
                        dimnames(ivf)$INSTR)
ivf_a = ivf[, asset_items, , , , , ]

# Liability items: L30 → F52 (fund shares issued)
liab_items = intersect(c('L30'), dimnames(ivf)$INSTR)
ivf_l = ivf[, liab_items, , , , , ]

rm(ivf); gc()

# ==============================================================================
# 12. INSTRUMENT RENAME
# ==============================================================================

# --- Assets ---
dict_instr_a = c('A20' = 'F4',  'A30' = 'F3',   'A50' = 'F5',
                 'A5A' = 'F51', 'A51' = 'F511', 'A52' = 'F52',
                 'L20' = 'F2M')
dimnames(ivf_a)$INSTR = dict_instr_a[dimnames(ivf_a)$INSTR]

# --- Liabilities ---
# NOTE: INSTR dimension dropped automatically when subsetting to single L30.
# ivf_l has 6 dims: REF_AREA, MATURITY, STO, COUNTERPART_AREA, COUNTERPART_SECTOR, TIME
# The instrument (F52) is implicit and specified in the filler LHS.
# Dimension names match ll directly — no renaming needed:
#   REF_AREA         → ll REF_AREA (dim 8): fund country (issuer)
#   COUNTERPART_AREA → ll COUNTERPART_AREA (dim 2): holder geography
#   COUNTERPART_SECTOR → ll COUNTERPART_SECTOR (dim 3): holder sector
#   S124 hardcoded on filler LHS → ll REF_SECTOR (dim 4): fund sector

# ==============================================================================
# 13. SAVE
# ==============================================================================

saveRDS(ivf_a, file.path(data_dir, 'ivf_assets.rds'))
saveRDS(ivf_l, file.path(data_dir, 'ivf_liab.rds'))

saveRDS(ivf_a, file.path(data_dir, 'vintages/ivf_assets_' %&% format(Sys.time(), '%F') %&% '_.rds'))
saveRDS(ivf_l, file.path(data_dir, 'vintages/ivf_liab_'   %&% format(Sys.time(), '%F') %&% '_.rds'))

rm(ivfrawq, ivfrawm, ivfraw, ivf_a, ivf_l); gc()