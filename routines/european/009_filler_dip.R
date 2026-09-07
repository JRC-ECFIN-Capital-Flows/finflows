# 009_filler_dip.R
# Fills aa (assets) and ll (liabilities) with IMF DIP data (formerly CDIS)
# Processes country-by-country to avoid memory issues
# Runs AFTER OECD FDI (006_filler_fdi): DIP is a strict second-best source,
# onlyna = TRUE throughout, so it only reaches reporters for which neither
# Eurostat BOP/IIP nor OECD FDI provided a value.
# -----------------------------------------------------------
#
# INPUTS : aa_iip_score.rds / ll_iip_score.rds
# OUTPUTS: aa_iip_dip.rds   / ll_iip_dip.rds
#
# KNOWN MD3 QUIRK: any subsetting operation (string-based or positional)
# drops ALL singleton dimensions. This script calls ensure_dims() after
# every subsetting step to re-add them and guarantee a fixed dim structure.
# (same convention as 005_filler_pip.R)
# -----------------------------------------------------------
#
# =========================== DECISIONS ====================================
# All choices below were agreed explicitly; documented here so the mapping
# never has to be reconstructed.
#
# 1. SOURCE POSITION. DIP is loaded just after OECD FDI. Every fill is
#    onlyna = TRUE. It therefore only lands where BOP/IIP and OECD are both
#    absent (the ~40 DIP-only reporters: HKG, BHR, KWT, MAC, ...).
#
# 2. ARCHITECTURE. PIP-style: country-by-country from dipbuffer/dip_<cc>.rds,
#    plus a synthetic EA20 aggregate built from EA member caches. (The EA
#    aggregate will rarely bind, since EA members are covered upstream, but
#    it is built for completeness / symmetry with PIP.)
#
# 3. DV_TYPE. Collapsed as the FIRST structural step: take O (officially
#    reported), backfill NA with SCC (derived/mirror). Single object forward.
#
# 4. FREQUENCY. DIP TIME is annual (year-end LE stock). Handled exactly like
#    the FDI/PIP annual branch: frequency(x) <- 'Q'.
#
# 5. CURRENCY / SCALE. DIP is IMF (like PIP) and reported in USD *units*.
#    Convert with / exr then / 1e6, round to 2  ==  PIP's /(usd_eur*1e6).
#    (NOT the FDI/OECD /usd_eur, which assumes USD millions.)
#
# 6. INSTRUMENTS (STO = LE, FUNCTIONAL_CAT = _D throughout):
#      F4 assets      = OTWD_D_AG_FL_ALL + INWD_D_AG_FL_ALL   (gross, both directions)
#      F4 liabilities = OTWD_D_LG_FL_ALL + INWD_D_LG_FL_ALL   (gross, both directions)
#      F5 assets      = OTWD_D_NETAL_F51_ALL                  (net equity, outward)
#      F5 liabilities = INWD_D_NETLA_F51_ALL                  (net equity, inward)
#    The IMF "F51" aggregate already contains the F52 part, so it maps to
#    Finflows INSTR = F5 (same convention as PIP's P_F51_P_USD -> F5).
#    F (total), F5A/F5B (no reinvested-earnings split in DIP), and every
#    _FE (fellow-enterprise) code are DROPPED.
#    F5 note: OECD publishes only the outward (asset) equity leg; DIP carries
#    both. Under onlyna-after-OECD, DIP F5 only reaches reporters OECD misses
#    (accepted: option (a), no zero-overwrite of OECD liabilities).
#
# 7. RFI -> S12 (resident financial intermediaries), debt only, as-is:
#      F4 assets,      REF_SECTOR = S12        <- OTWD_D_NETAL_FL_RFI
#      F4 liabilities, COUNTERPART_SECTOR = S12<- INWD_D_NETLA_FL_RFI
#    Placed at their NET level, NO scaling to the gross S1 total. Consequence
#    (documented, not a bug): the S12 debt cell is net while the S1 F4 total
#    is gross, so S12 is NOT a clean subset of S1 (non-additive), and net FI
#    positions can legitimately be negative in an LE slot. REXFI is DROPPED.
#
# 8. SECTORS. Everything else is S1.S1 (OECD/DIP direct investment carries no
#    sector breakdown), except the RFI S12 cells above.
#
# 9. MIRROR. NONE. aa is filled from the outward/asset indicators and ll from
#    the inward/liability indicators, each indexed by the reporter. DIP gives
#    both directions natively, so no asset<->liability mirror is computed
#    (unlike PIP). Direct fills only.
#
# 10. CODED AREAS. DIP reporters/counterparts are ISO3; ordinary ones resolve
#     via ccode(iso3c -> iso2m). Non-ISO3 IMF aggregate codes are kept
#     verbatim (ccode leaveifNA) and, if absent from aa/ll, are added by the
#     MD3 assignment itself. Documented reporter aggregates (labels for
#     reference, so this need not be looked up again):
#         G001  World                              (already a valid REF_AREA)
#         GX225 North and Central America
#         GX226 North Atlantic and Caribbean
#         GX451 Economies of the Persian Gulf
#         GX454 Other Near and Middle East economies
#         GX509 Central and South Asia
#         GX510 East Asia
#         GX641 North Africa
#         GX837 Oceania and Polar regions
#     Same keep-verbatim rule applies to coded counterparts (TX###, U###,
#     GX### on the counterpart side).
#
# 11. CN. Counterpart CN -> CN_X_HK after ccode, exactly as PIP (HKG is a
#     separate counterpart, so CHN is treated as excluding Hong Kong).
# ==========================================================================

library(MDstats); library(MD3); library(data.table)

# Set data directory
if (!exists("data_dir"))   data_dir   = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/filled'
if (!exists("loaded_dir")) loaded_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/loaded'

source('\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/finflows/routines/utilities.R')

gc()

#############################################################################
# --- Load aa, ll (score = input to the DIP step) ---
#############################################################################

aa <- readRDS(file.path(data_dir, 'aa_iip_score.rds')); gc()
ll <- readRDS(file.path(data_dir, 'll_iip_score.rds')); gc()

#############################################################################
# --- Load DIP country list ---
#############################################################################
gc()
whatctries <- readRDS(file.path(loaded_dir, 'dipbuffer/whatctries_dip.rds'))
ccc  <- dimcodes(whatctries)[[1]]
AREA <- ccc[, 1]                       # ISO3 (and coded) reporter list
AREA <- unique(c(AREA, "USA"))         # ensure US present (mirrors PIP)
rm(whatctries); gc()

#############################################################################
# --- Exchange rate: annual USD->EUR, fetch once before the loop ---
# DIP is annual only, so (unlike PIP) no semi-annual leg is needed.
#############################################################################

exr <- mds('ECB/EXR/A.USD.EUR.SP00.E')

#############################################################################
# --- DIP indicator map: INDICATOR -> (SIDE, INSTR, REF_SECTOR, COUNTERPART_SECTOR) ---
# SIDE : "A" -> fills aa ; "L" -> fills ll
# Both sectors are stated explicitly per indicator, so nothing downstream has
# to reinterpret a sector by side. Everything is S1.S1 except the RFI cells:
#   RFI assets  -> REF_SECTOR = S12 (resident FI holds the asset), CP = S1
#   RFI liab    -> COUNTERPART_SECTOR = S12 (resident FI is the counterpart), REF = S1
# Only the six economic targets below are used; all other indicators dropped.
#
# F4 totals are gross and sum the two DIRECTIONS (OTWD + INWD) of the same
# leg: AG (asset gross) for assets, LG (liability gross) for liabilities.
#############################################################################

dip_map <- data.table(
  INDICATOR = c("OTWD_D_AG_FL_ALL",    "INWD_D_AG_FL_ALL",    # -> F4 assets  S1.S1
                "OTWD_D_LG_FL_ALL",    "INWD_D_LG_FL_ALL",    # -> F4 liab    S1.S1
                "OTWD_D_NETAL_F51_ALL",                        # -> F5 assets  S1.S1
                "INWD_D_NETLA_F51_ALL",                        # -> F5 liab    S1.S1
                "OTWD_D_NETAL_FL_RFI",                         # -> F4 assets  S12.S1
                "INWD_D_NETLA_FL_RFI"),                        # -> F4 liab    S1.S12
  SIDE               = c("A", "A", "L", "L", "A", "L", "A",  "L"),
  INSTR              = c("F4","F4","F4","F4","F5","F5","F4", "F4"),
  REF_SECTOR         = c("S1","S1","S1","S1","S1","S1","S12","S1"),
  COUNTERPART_SECTOR = c("S1","S1","S1","S1","S1","S1","S1", "S12")
)

#############################################################################
# --- EA membership by period (ISO3 codes) --- (identical to PIP)
#############################################################################

ea_members_base <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA",
                     "GRC", "IRL", "ITA", "LUX", "NLD", "PRT")

ea_members_base_iso2 <- c("AT", "BE", "DE", "ES", "FI", "FR",
                          "GR", "IE", "IT", "LU", "NL", "PT")

ea_transitions <- list(
  list(join_year = 2007, codes = "SVN",           iso2 = "SI",         label = "EA13"),
  list(join_year = 2008, codes = c("CYP", "MLT"), iso2 = c("CY","MT"), label = "EA15"),
  list(join_year = 2009, codes = "SVK",           iso2 = "SK",         label = "EA16"),
  list(join_year = 2011, codes = "EST",           iso2 = "EE",         label = "EA17"),
  list(join_year = 2014, codes = "LVA",           iso2 = "LV",         label = "EA18"),
  list(join_year = 2015, codes = "LTU",           iso2 = "LT",         label = "EA19"),
  list(join_year = 2023, codes = "HRV",           iso2 = "HR",         label = "EA20"),
  list(join_year = 2026, codes = "BGR",           iso2 = "BG",         label = "EA21")
)

ea_members_for_year <- function(year) {
  members <- ea_members_base
  for (tr in ea_transitions) if (year >= tr$join_year) members <- c(members, tr$codes)
  members
}
ea_members_iso2_for_year <- function(year) {
  members <- ea_members_base_iso2
  for (tr in ea_transitions) if (year >= tr$join_year) members <- c(members, tr$iso2)
  members
}
ea_label_for_year <- function(year) {
  label <- "EA12"
  for (tr in ea_transitions) if (year >= tr$join_year) label <- tr$label
  label
}

# extract year from quarterly time code (e.g. "2024q4" -> 2024)
year_from_q <- function(tc) as.integer(sub("q[1-4]$", "", tc))

#############################################################################
# --- Helper: ensure exactly the expected dims exist, in order --- (from PIP)
#############################################################################

ensure_dims <- function(obj, dim_names, dim_codes) {
  for (dname in dim_names) {
    if (!(dname %in% names(dimnames(obj)))) {
      obj <- add.dim(obj, .dimname = dname,
                     .dimcodes = dim_codes[[dname]],
                     .fillall = FALSE)
    }
  }
  obj <- aperm(obj, dim_names)
  return(obj)
}

#############################################################################
# --- Helper: process one country's DIP cache into long data.tables ---
# Returns list(dt_a, dt_l): tidy tables with columns
#   INSTR, REF_SECTOR, COUNTERPART_SECTOR, COUNTERPART_AREA, TIME, value
# ready to be turned into MD3 and filled. (assets in dt_a, liabilities dt_l)
#
# Steps: DV_TYPE collapse -> USD->EUR/1e6/round -> freq A->Q ->
#        indicator map + two-direction sum -> ISO3->ISO2 counterpart recode.
#############################################################################

process_dip_country <- function(dip_cc) {
  
  dv <- dimnames(dip_cc)$DV_TYPE
  
  # dims after dropping COUNTRY (singleton) + DV_TYPE (sliced):
  keep_names <- c("INDICATOR", "COUNTERPART_COUNTRY", "TIME")
  keep_codes <- list(
    INDICATOR           = dimnames(dip_cc)$INDICATOR,
    COUNTERPART_COUNTRY = dimnames(dip_cc)$COUNTERPART_COUNTRY
  )
  
  # --- 3. DV_TYPE collapse: O, backfilled by SCC ---
  if ("O" %in% dv) {
    dip1 <- ensure_dims(dip_cc[, "O", , , ], keep_names, keep_codes)
    if ("SCC" %in% dv) {
      scc  <- ensure_dims(dip_cc[, "SCC", , , ], keep_names, keep_codes)
      dip1[onlyna = TRUE] <- scc
      rm(scc)
    }
  } else if ("SCC" %in% dv) {
    dip1 <- ensure_dims(dip_cc[, "SCC", , , ], keep_names, keep_codes)
  } else {
    return(list(dt_a = NULL, dt_l = NULL))
  }
  
  # --- 5. Currency: USD units -> EUR, then MEUR (PIP convention) ---
  dip1 <- dip1 / exr
  dip1 <- dip1 / 1e6
  dip1 <- round(dip1, 2)
  
  # --- 4. Frequency: annual -> quarterly ---
  frequency(dip1) <- 'Q'
  
  # --- to long data.table ---
  dt <- as.data.table(as.data.frame(dip1))
  valcol <- setdiff(names(dt), c("INDICATOR", "COUNTERPART_COUNTRY", "TIME"))
  if (length(valcol) == 1) setnames(dt, valcol, "value")
  dt <- dt[!is.na(value)]
  if (nrow(dt) == 0) return(list(dt_a = NULL, dt_l = NULL))
  
  # --- 6/7. Map indicators; drop everything unmapped. Both sectors come
  #          straight from dip_map, so no side-dependent reinterpretation. ---
  dt <- dt[dip_map, on = "INDICATOR", nomatch = NULL,
           .(SIDE, INSTR, REF_SECTOR, COUNTERPART_SECTOR,
             COUNTERPART_COUNTRY, TIME, value)]
  if (nrow(dt) == 0) return(list(dt_a = NULL, dt_l = NULL))
  
  # --- two-direction sum (OTWD + INWD collapse into one target cell);
  #     F5 / RFI single-code rows pass through unchanged ---
  dt <- dt[, .(value = sum(value, na.rm = TRUE)),
           by = .(SIDE, INSTR, REF_SECTOR, COUNTERPART_SECTOR,
                  COUNTERPART_COUNTRY, TIME)]
  
  # --- 10/11. Counterpart ISO3 -> ISO2 (coded aggregates kept verbatim) ---
  dt[, COUNTERPART_AREA := ccode(COUNTERPART_COUNTRY, 'iso3c', 'iso2m',
                                 leaveifNA = TRUE, warn = FALSE)]
  dt[COUNTERPART_AREA == 'CN', COUNTERPART_AREA := 'CN_X_HK']
  dt[, COUNTERPART_COUNTRY := NULL]
  
  dt_a <- dt[SIDE == "A", .(INSTR, REF_SECTOR, COUNTERPART_SECTOR,
                            COUNTERPART_AREA, TIME, value)]
  dt_l <- dt[SIDE == "L", .(INSTR, REF_SECTOR, COUNTERPART_SECTOR,
                            COUNTERPART_AREA, TIME, value)]
  
  if (nrow(dt_a) == 0) dt_a <- NULL
  if (nrow(dt_l) == 0) dt_l <- NULL
  list(dt_a = dt_a, dt_l = dt_l)
}

#############################################################################
# --- Helper: long data.table -> MD3 with the 5-dim fill layout ---
#   dims: INSTR . REF_SECTOR . COUNTERPART_SECTOR . COUNTERPART_AREA . TIME
#############################################################################

dt_to_md3 <- function(dt) {
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  key_cols <- c("INSTR", "REF_SECTOR", "COUNTERPART_SECTOR",
                "COUNTERPART_AREA", "TIME")
  dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = key_cols]
  dim_struct <- lapply(key_cols, function(col) sort(unique(dt[[col]])))
  names(dim_struct) <- key_cols
  as.md3(dt, id.vars = key_cols, dcstruct = dim_struct,
         timeid = "TIME", na.rm = TRUE)
}

#############################################################################
# --- EA20 aggregate: sum EA member caches, exclude intra-EA counterparts ---
# Same data.table approach as PIP. Rarely binds (members covered upstream),
# built for completeness.
#############################################################################

cat('=== Computing EA aggregate from member country DIP caches ===\n')

ea_all_members <- ea_members_for_year(2100)

ea_available <- character(0)
for (m in ea_all_members) {
  cachefile <- file.path(loaded_dir, 'dipbuffer/dip_' %&% m %&% '.rds')
  if (file.exists(cachefile)) ea_available <- c(ea_available, m)
}
cat('  EA members with caches:', length(ea_available), '/', length(ea_all_members), '\n')

if (length(ea_available) > 0) {
  
  dt_all_a <- list(); dt_all_l <- list()
  
  for (m in ea_available) {
    cat('  ', m, '... ')
    dip_cc <- readRDS(file.path(loaded_dir, 'dipbuffer/dip_' %&% m %&% '.rds'))
    res <- process_dip_country(dip_cc)
    rm(dip_cc); gc()
    
    if (!is.null(res$dt_a)) { d <- copy(res$dt_a); d[, MEMBER := m]; dt_all_a[[m]] <- d; rm(d) }
    if (!is.null(res$dt_l)) { d <- copy(res$dt_l); d[, MEMBER := m]; dt_all_l[[m]] <- d; rm(d) }
    rm(res); gc()
    cat('done\n')
  }
  
  dt_a <- if (length(dt_all_a) > 0) rbindlist(dt_all_a) else NULL
  dt_l <- if (length(dt_all_l) > 0) rbindlist(dt_all_l) else NULL
  rm(dt_all_a, dt_all_l); gc()
  
  if (!is.null(dt_a)) dt_a[, YEAR := year_from_q(TIME)]
  if (!is.null(dt_l)) dt_l[, YEAR := year_from_q(TIME)]
  
  all_years_in_data <- sort(unique(c(
    if (!is.null(dt_a)) dt_a$YEAR else integer(0),
    if (!is.null(dt_l)) dt_l$YEAR else integer(0)
  )))
  
  # --- exclude intra-EA counterpart rows (per-year EA composition) ---
  ea_membership_dt <- rbindlist(lapply(all_years_in_data, function(yr)
    data.table(YEAR = yr, EA_ISO2 = ea_members_iso2_for_year(yr))))
  
  if (!is.null(dt_a)) {
    dt_a[, is_intra := FALSE]
    dt_a[ea_membership_dt, is_intra := TRUE,
         on = .(YEAR = YEAR, COUNTERPART_AREA = EA_ISO2)]
    dt_a <- dt_a[is_intra == FALSE][, is_intra := NULL]
  }
  if (!is.null(dt_l)) {
    dt_l[, is_intra := FALSE]
    dt_l[ea_membership_dt, is_intra := TRUE,
         on = .(YEAR = YEAR, COUNTERPART_AREA = EA_ISO2)]
    dt_l <- dt_l[is_intra == FALSE][, is_intra := NULL]
  }
  rm(ea_membership_dt); gc()
  
  # --- label + keep only members belonging to EA in that year ---
  if (!is.null(dt_a)) dt_a[, EA_LABEL := sapply(YEAR, ea_label_for_year)]
  if (!is.null(dt_l)) dt_l[, EA_LABEL := sapply(YEAR, ea_label_for_year)]
  
  ea_member_year_dt <- rbindlist(lapply(all_years_in_data, function(yr)
    data.table(YEAR = yr, MEMBER = ea_members_for_year(yr))))
  
  if (!is.null(dt_a)) dt_a <- dt_a[ea_member_year_dt, on = .(YEAR, MEMBER), nomatch = NULL]
  if (!is.null(dt_l)) dt_l <- dt_l[ea_member_year_dt, on = .(YEAR, MEMBER), nomatch = NULL]
  rm(ea_member_year_dt); gc()
  
  key_cols <- c("INSTR", "REF_SECTOR", "COUNTERPART_SECTOR", "COUNTERPART_AREA", "TIME")
  
  ea_labels_a <- if (!is.null(dt_a)) unique(dt_a$EA_LABEL) else character(0)
  ea_labels_l <- if (!is.null(dt_l)) unique(dt_l$EA_LABEL) else character(0)
  all_ea_labels <- sort(unique(c(ea_labels_a, ea_labels_l)))
  cat('  EA labels to fill:', paste(all_ea_labels, collapse = ', '), '\n')
  
  for (lbl in all_ea_labels) {
    cat('  Building MD3 for', lbl, '... ')
    sub_a <- if (!is.null(dt_a) && lbl %in% dt_a$EA_LABEL)
      dt_a[EA_LABEL == lbl, .(INSTR, REF_SECTOR, COUNTERPART_SECTOR,
                              COUNTERPART_AREA, TIME, value)] else NULL
    sub_l <- if (!is.null(dt_l) && lbl %in% dt_l$EA_LABEL)
      dt_l[EA_LABEL == lbl, .(INSTR, REF_SECTOR, COUNTERPART_SECTOR,
                              COUNTERPART_AREA, TIME, value)] else NULL
    dip_a <- dt_to_md3(sub_a)
    dip_l <- dt_to_md3(sub_l)
    if (!is.null(dip_a)) {
      aa[, lbl, , , "LE", "_D", , , usenames = TRUE, onlyna = TRUE] <-
        aperm(dip_a, c(1, 2, 3, 5, 4))
    }
    if (!is.null(dip_l)) {
      ll[, , , , "LE", "_D", , lbl, usenames = TRUE, onlyna = TRUE] <-
        aperm(dip_l, c(1, 4, 3, 2, 5))
    }
    rm(sub_a, sub_l, dip_a, dip_l); gc()
    cat('done\n')
  }
  
  rm(dt_a, dt_l); gc()
  cat('  EA aggregate: done\n')
  
} else {
  cat('  WARNING: No EA member caches found - skipping EA aggregate\n')
}

#############################################################################
# --- Main loop: process each reporter and fill aa, ll ---
#############################################################################

cat('\n=== Processing DIP data for', length(AREA), 'reporters ===\n')

for (cc in AREA) {
  
  cachefile <- file.path(loaded_dir, 'dipbuffer/dip_' %&% cc %&% '.rds')
  if (!file.exists(cachefile)) { cat(cc, ': no cache file, skipping\n'); next }
  
  cat(as.character(Sys.time()), ': ', match(cc, AREA), '/', length(AREA),
      ' ', cc, '... ')
  
  dip_cc <- readRDS(cachefile)
  
  # reporter ISO2 (coded aggregates kept verbatim via leaveifNA)
  cc2 <- ccode(cc, 'iso3c', 'iso2m', leaveifNA = TRUE, warn = FALSE)
  if (cc2 == 'CN') cc2 <- 'CN_X_HK'
  
  res <- process_dip_country(dip_cc)
  rm(dip_cc); gc()
  
  dip_a <- dt_to_md3(res$dt_a)
  dip_l <- dt_to_md3(res$dt_l)
  if (!is.null(dip_a)) {
    aa[, cc2, , , "LE", "_D", , , usenames = TRUE, onlyna = TRUE] <-
      aperm(dip_a, c(1, 2, 3, 5, 4))
  }
  if (!is.null(dip_l)) {
    ll[, , , , "LE", "_D", , cc2, usenames = TRUE, onlyna = TRUE] <-
      aperm(dip_l, c(1, 4, 3, 2, 5))
  }
  
  rm(res, dip_a, dip_l); gc()
  cat('done\n')
}

#############################################################################
# --- Save outputs ---
#############################################################################

saveRDS(aa, file.path(data_dir, 'aa_iip_dip.rds'))
saveRDS(ll, file.path(data_dir, 'll_iip_dip.rds'))

saveRDS(aa, file.path(data_dir, 'vintages/aa_iip_dip_' %&% format(Sys.time(), '%F') %&% '_.rds'))
saveRDS(ll, file.path(data_dir, 'vintages/ll_iip_dip_' %&% format(Sys.time(), '%F') %&% '_.rds'))

cat('Done. Saved aa_iip_dip.rds and ll_iip_dip.rds\n')