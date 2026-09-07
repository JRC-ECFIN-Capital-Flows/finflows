# ==============================================================================
# 004_z_filler_ecb_ivf.R
# Fill aa and ll from ECB IVF (Investment Fund Statistics)
# Reference sector: S124 (investment funds)
# Runs after 004_filler_shss.R; reads aa_iip_shss / ll_iip_shss
#
# ASSET FILLING (aa):
#   F4/F4S/F4L  — loans, maturity-split
#   F3/F3S/F3L  — debt securities, maturity-split
#   F5          — equity + IF shares (total)
#   F51         — equity
#   F511        — listed shares
#   F52         — IF shares (direct + derived as F5 − F51)
#   F2M         — deposits held by funds
#
# LIABILITY FILLING (ll):
#   F52         — fund shares issued (L30), no swap needed
#
# FUNCTIONAL_CAT: _T only (classification TBD — check with Erza)
# ==============================================================================

library(MDstats)
library(MD3)

if (!exists("data_dir"))   data_dir   = getwd()
if (!exists("loaded_dir")) loaded_dir = data_dir

`%&%` = function (..., collapse = NULL, recycle0 = FALSE) .Internal(paste0(list(...), collapse, recycle0))

# ==============================================================================
# 1. LOAD pipeline state and IVF data
# ==============================================================================

aa = readRDS(file.path(data_dir, 'aa_iip_shss.rds')); gc()
ll = readRDS(file.path(data_dir, 'll_iip_shss.rds')); gc()

ivf_assets = readRDS(file.path(loaded_dir, 'ivf_assets.rds')); gc()
ivf_liab   = readRDS(file.path(loaded_dir, 'ivf_liab.rds'));   gc()

# ==============================================================================
# 2. ASSET-SIDE FILLING (aa)
#    Target: aa[INSTR . REF_AREA . S124 . COUNTERPART_SECTOR . STO . _T . TIME . COUNTERPART_AREA]
#    Source: ivf_assets[REF_AREA . INSTR . MATURITY . STO . COUNTERPART_AREA . COUNTERPART_SECTOR . TIME]
#
#    Maturity in source: A = total, F = short-term, K = long-term
#    Singleton INSTR and MATURITY dimensions drop after subsetting,
#    leaving REF_AREA, STO, COUNTERPART_AREA, COUNTERPART_SECTOR, TIME
#    which match aa's free dimensions via usenames=TRUE.
# ==============================================================================

# --- Loans (F4): maturity-split ---
aa[F4..S124..._T..,  usenames = TRUE, onlyna = TRUE] = ivf_assets['.F4.A....']
aa[F4S..S124..._T.., usenames = TRUE, onlyna = TRUE] = ivf_assets['.F4.F....']
aa[F4L..S124..._T.., usenames = TRUE, onlyna = TRUE] = ivf_assets['.F4.K....']

# --- Debt securities (F3): maturity-split ---
aa[F3..S124..._T..,  usenames = TRUE, onlyna = TRUE] = ivf_assets['.F3.A....']
aa[F3S..S124..._T.., usenames = TRUE, onlyna = TRUE] = ivf_assets['.F3.F....']
aa[F3L..S124..._T.., usenames = TRUE, onlyna = TRUE] = ivf_assets['.F3.K....']

# --- Equity and IF shares: maturity A only ---
aa[F5..S124..._T..,   usenames = TRUE, onlyna = TRUE] = ivf_assets['.F5.A....']
aa[F51..S124..._T..,  usenames = TRUE, onlyna = TRUE] = ivf_assets['.F51.A....']
aa[F511..S124..._T.., usenames = TRUE, onlyna = TRUE] = ivf_assets['.F511.A....']
aa[F52..S124..._T..,  usenames = TRUE, onlyna = TRUE] = ivf_assets['.F52.A....']

# --- Deposits held by funds (F2M): maturity A only ---
aa[F2M..S124..._T.., usenames = TRUE, onlyna = TRUE] = ivf_assets['.F2M.A....']

# --- Derive F52 = F5 - F51 where direct F52 is still missing ---
aa[F52..S124..._T.., onlyna = TRUE] = aa[F5..S124..._T..] - aa[F51..S124..._T..]

# --- Derive F51M = F51 - F511 (unlisted shares) where F51M is missing ---
aa[F51M..S124..._T.., onlyna = TRUE] = aa[F51..S124..._T..] - aa[F511..S124..._T..]

gc()

# ==============================================================================
# 3. LIABILITY-SIDE FILLING (ll)
#    L30 → F52 (fund shares issued by S124)
#
#    ivf_liab has 6 dims (INSTR dropped as singleton in loader):
#      REF_AREA . MATURITY . STO . COUNTERPART_AREA . COUNTERPART_SECTOR . TIME
#
#    Mapping to ll (INSTR.COUNTERPART_AREA.COUNTERPART_SECTOR.REF_SECTOR.STO.FUNCTIONAL_CAT.TIME.REF_AREA):
#      REF_AREA (fund country, issuer)         → ll REF_AREA (dim 8)
#      COUNTERPART_AREA (holder geography)     → ll COUNTERPART_AREA (dim 2)
#      COUNTERPART_SECTOR (holder sector)      → ll COUNTERPART_SECTOR (dim 3)
#      S124 hardcoded on LHS                   → ll REF_SECTOR (dim 4)
#    No swap needed — dimension names already match ll directly.
# ==============================================================================

# Fill ll: select MATURITY = A (total); after singleton drop,
# remaining dims: REF_AREA, STO, COUNTERPART_AREA, COUNTERPART_SECTOR, TIME
# which match ll's free dimensions via usenames=TRUE.
ll[F52...S124.._T.., usenames = TRUE, onlyna = TRUE] = ivf_liab['.A....']

gc()

# ==============================================================================
# 4. SAVE
# ==============================================================================

saveRDS(aa, file.path(data_dir, 'aa_iip_ivf.rds'))
saveRDS(ll, file.path(data_dir, 'll_iip_ivf.rds'))

saveRDS(aa, file.path(data_dir, 'vintages/aa_iip_ivf_' %&% format(Sys.time(), '%F') %&% '_.rds'))
saveRDS(ll, file.path(data_dir, 'vintages/ll_iip_ivf_' %&% format(Sys.time(), '%F') %&% '_.rds'))