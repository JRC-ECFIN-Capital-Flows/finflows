# filler_pip.R
# Fills aa (assets) and ll (liabilities) with PIP data (formerly CPIS)
# Processes country-by-country to avoid memory issues
# Finflows 3.0
# -----------------------------------------------------------
#
# KNOWN MD3 QUIRK: any subsetting operation (string-based or positional)
# drops ALL singleton dimensions. This script calls ensure_dims() after
# every subsetting step to re-add them and guarantee a fixed dim structure.
# -----------------------------------------------------------
#
# CHANGES vs prior version:
#   1. OFM → S124 recoding (after dim rename, before freq adjustment)
#   2. Computed aggregates: S12T, S12K, S12O, S1M (per-country, before fill)
#   3. EA aggregate: sum individual EA member country caches by time-varying
#      composition (EA18→EA19→EA20→EA21), processed through same filler logic
# -----------------------------------------------------------

library(MDstats); library(MD3); library(data.table)

# Set data directory
if (!exists("data_dir")) data_dir = getwd()
if (!exists("loaded_dir")) loaded_dir = data_dir

source('V:/FinFlows/githubrepo/finflows/routines/utilities.R')

gc()

#############################################################################
# --- Load aa, ll ---
#############################################################################

aa <- readRDS(file.path(data_dir, 'aa_iip_ivf.rds')); gc()
ll <- readRDS(file.path(data_dir, 'll_iip_ivf.rds')); gc()

#############################################################################
# --- Load PIP country list ---
#############################################################################
gc()
gc()
whatctries <- readRDS(file.path(loaded_dir, 'pipbuffer/whatctries_pip.rds'))
ccc <- dimcodes(whatctries)[[1]]
AREA <- ccc[, 1]  # ISO3 codes
AREA <- c(ccc[, 1], "USA") ### adding US that was lacking
rm(whatctries); gc()

#############################################################################
# --- Exchange rate: fetch once before the loop ---
#############################################################################

exra <- mds('ECB/EXR/A.USD.EUR.SP00.E')
exrs <- mds('ECB/EXR/Q.USD.EUR.SP00.E')
exrs <- aggregate(exrs, 'S', FUN = end)
exr  <- merge(exra, exrs)
rm(exra, exrs); gc()

#############################################################################
# --- Indicator recoding map ---
#############################################################################

map_indicator <- c(
  "P_TOTINV_P_USD" = "F",
  "P_F51_P_USD"    = "F5",    ####attention, the IMF calls F51 the aggregate which also includes IF parts, that is F51+F52=F5. THIS IS TO BE ADDED TO THE DOCUMENTATION.
  "P_F3_P_USD"     = "F3",
  "P_F3_S_P_USD"   = "F3S",
  "P_F3_L_P_USD"   = "F3L"
)

#############################################################################
# --- EA membership by period (ISO3 codes) ---
#############################################################################
# Each entry: list of ISO3 codes added at that transition
# Time ranges use semi-annual end-of-period convention (PIP frequency)

# ISO3 codes for EA12 (founding members from 2001)
ea_members_base <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA",
                     "GRC", "IRL", "ITA", "LUX", "NLD", "PRT")

# ISO2 codes for EA12 (for intra-EA counterpart exclusion, post ccode conversion)
ea_members_base_iso2 <- c("AT", "BE", "DE", "ES", "FI", "FR",
                          "GR", "IE", "IT", "LU", "NL", "PT")

ea_transitions <- list(
  list(join_year = 2007, codes = "SVN",          iso2 = "SI", label = "EA13"),
  list(join_year = 2008, codes = c("CYP", "MLT"), iso2 = c("CY", "MT"), label = "EA15"),
  list(join_year = 2009, codes = "SVK",          iso2 = "SK", label = "EA16"),
  list(join_year = 2011, codes = "EST",          iso2 = "EE", label = "EA17"),
  list(join_year = 2014, codes = "LVA",          iso2 = "LV", label = "EA18"),
  list(join_year = 2015, codes = "LTU",          iso2 = "LT", label = "EA19"),
  list(join_year = 2023, codes = "HRV",          iso2 = "HR", label = "EA20"),
  list(join_year = 2026, codes = "BGR",          iso2 = "BG", label = "EA21")
)

# For a given year, return vector of EA member ISO3 codes
ea_members_for_year <- function(year) {
  members <- ea_members_base
  for (tr in ea_transitions) {
    if (year >= tr$join_year) members <- c(members, tr$codes)
  }
  members
}

# For a given year, return vector of EA member ISO2 codes
ea_members_iso2_for_year <- function(year) {
  members <- ea_members_base_iso2
  for (tr in ea_transitions) {
    if (year >= tr$join_year) members <- c(members, tr$iso2)
  }
  members
}

# For a given year, return the Finflows EA label (EA12, EA13, ..., EA21)
ea_label_for_year <- function(year) {
  label <- "EA12"
  for (tr in ea_transitions) {
    if (year >= tr$join_year) label <- tr$label
  }
  label
}

#############################################################################
# --- Helper: ensure exactly the expected dims exist, in order ---
# Re-adds any singleton dims that MD3 dropped, then reorders.
# Works with EITHER pre-rename (PIP) or post-rename (Finflows) names.
#############################################################################

ensure_dims <- function(obj, dim_names, dim_codes) {
  # dim_names: character vector of expected dimension names, in order
  # dim_codes: named list mapping dim_name -> character vector of codes
  #            (used to re-add dropped singletons)
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
# --- Helper: string-based TIME subsetting ---
# Builds the right dot-prefix for any number of dims, subsets TIME
# at the last position, then re-ensures dimensions.
#############################################################################

subset_time <- function(obj, time_codes, dim_names, dim_codes) {
  ndim <- length(dim(obj))
  dot_prefix <- paste(rep("", ndim), collapse = ".")
  obj <- obj[dot_prefix %&% paste(time_codes, collapse = "+")]
  obj <- ensure_dims(obj, dim_names, dim_codes)
  return(obj)
}


#############################################################################
# --- Helper: compute sector aggregates on a processed pip object ---
# Expects an MD3 object with REF_SECTOR as dim 2, after OFM→S124 recode.
# Adds S12T, S12K, S12O, S1M as new codes in REF_SECTOR.
#
# S123 and S15 are zerofilled before aggregation (decision 2026-03-10
# with Erza): when these sectors have NA, treat as 0.
# S124 is NOT zerofilled — data is expected from a prior filling step.
#############################################################################

# Zerofiller: fills NA positions with a scalar (default 0)
zerofiller <- function(x, fillscalar = 0) {
  temp <- copy(x)
  temp[onlyna = TRUE] <- fillscalar
  temp
}

compute_sector_aggregates <- function(obj) {
  if (is.null(obj)) return(NULL)
  
  secs <- dimnames(obj)$REF_SECTOR
  dim_names <- names(dimnames(obj))
  
  # Helper: extract one REF_SECTOR slice, restoring ALL dropped singleton dims
  # (not just REF_SECTOR — e.g., GRC has singleton COUNTERPART_SECTOR=S1)
  all_dim_codes <- lapply(setdiff(dim_names, "TIME"), function(d) dimnames(obj)[[d]])
  names(all_dim_codes) <- setdiff(dim_names, "TIME")
  
  slice_sector <- function(obj, sector_code) {
    sliced <- obj["." %&% sector_code %&% "..."]
    dc <- all_dim_codes
    dc[["REF_SECTOR"]] <- sector_code  # override with the single code we sliced
    sliced <- ensure_dims(sliced, dim_names, dc)
    return(sliced)
  }
  
  # Pre-extract zerofilled slices for S123 and S15
  # (decision 2026-03-10 with Erza: treat NA as 0 for these two sectors)
  s123_zf <- if ("S123" %in% secs) zerofiller(slice_sector(obj, "S123")) else NULL
  s15_zf  <- if ("S15"  %in% secs) zerofiller(slice_sector(obj, "S15"))  else NULL
  
  # S12T = S122 + S123 (MFIs other than central bank)
  if (all(c("S122", "S123") %in% secs)) {
    s12t <- slice_sector(obj, "S122") + s123_zf
    dimnames(s12t)$REF_SECTOR <- "S12T"
    obj <- merge(obj, s12t)
    rm(s12t)
  }
  
  # S12K = S121 + S122 + S123 (All MFIs)
  if (all(c("S121", "S122", "S123") %in% secs)) {
    s12k <- slice_sector(obj, "S121") + slice_sector(obj, "S122") + s123_zf
    dimnames(s12k)$REF_SECTOR <- "S12K"
    obj <- merge(obj, s12k)
    rm(s12k)
  }
  
  # S12O = S12P - S124 (Other FI excl. MFIs, IC, PF, non-MMF IF)
  # S124 NOT zerofilled — expected from prior filling step
  if (all(c("S12P", "S124") %in% secs)) {
    s12o <- slice_sector(obj, "S12P") - slice_sector(obj, "S124")
    dimnames(s12o)$REF_SECTOR <- "S12O"
    obj <- merge(obj, s12o)
    rm(s12o)
  }
  
  # S1M = S14 + S15 (Households + NPISH)
  if (all(c("S14", "S15") %in% secs)) {
    s1m <- slice_sector(obj, "S14") + s15_zf
    dimnames(s1m)$REF_SECTOR <- "S1M"
    obj <- merge(obj, s1m)
    rm(s1m)
  }
  
  rm(s123_zf, s15_zf)
  return(obj)
}


#############################################################################
# --- Helper: process one country's PIP data (shared logic) ---
# Takes raw PIP cache object, returns list(pip_a, pip_l) ready for filling.
# Handles: USD→EUR, dim rename, OFM→S124, indicator recode, freq adj,
#          sector aggregates, time filter.
#############################################################################

process_pip_country <- function(pip_cc) {
  
  # Save original dim codes for re-adding dropped singletons (PIP names)
  orig_codes <- list(
    INDICATOR           = dimnames(pip_cc)$INDICATOR,
    SECTOR              = dimnames(pip_cc)$SECTOR,
    COUNTERPART_SECTOR  = dimnames(pip_cc)$COUNTERPART_SECTOR,
    COUNTERPART_COUNTRY = dimnames(pip_cc)$COUNTERPART_COUNTRY
  )
  orig_names <- c("INDICATOR", "SECTOR", "COUNTERPART_SECTOR",
                  "COUNTERPART_COUNTRY", "TIME")
  
  # --- Slice by ACCOUNTING_ENTRY ---
  has_assets <- "A" %in% dimnames(pip_cc)$ACCOUNTING_ENTRY
  has_liab   <- "L" %in% dimnames(pip_cc)$ACCOUNTING_ENTRY
  
  pip_a <- if (has_assets) ensure_dims(pip_cc[, "A", , , , , ], orig_names, orig_codes) else NULL
  pip_l <- if (has_liab)   ensure_dims(pip_cc[, "L", , , , , ], orig_names, orig_codes) else NULL
  
  # --- Process assets ---
  pip_a <- process_pip_side(pip_a)
  
  # --- Process liabilities ---
  pip_l <- process_pip_side(pip_l)
  
  return(list(pip_a = pip_a, pip_l = pip_l))
}


#############################################################################
# --- Helper: process one side (A or L) of a country's PIP data ---
#############################################################################

process_pip_side <- function(pip_side) {
  if (is.null(pip_side)) return(NULL)
  
  # Currency: USD -> EUR, then MEUR
  pip_side <- pip_side / exr
  pip_side <- pip_side / 1e6
  pip_side <- round(pip_side, 2)
  
  # Rename dimensions
  nms <- names(dimnames(pip_side))
  nms[nms == "INDICATOR"]           <- "INSTR"
  nms[nms == "SECTOR"]              <- "REF_SECTOR"
  nms[nms == "COUNTERPART_COUNTRY"] <- "COUNTERPART_AREA"
  names(dimnames(pip_side)) <- nms
  
  # Recode INSTR
  for (old in names(map_indicator)) {
    dimnames(pip_side)$INSTR[dimnames(pip_side)$INSTR == old] <- map_indicator[old]
  }
  
  # --- OFM → S124 recode ---
  dimnames(pip_side)$REF_SECTOR[
    dimnames(pip_side)$REF_SECTOR == "OFM"] <- "S124"
  
  # Convert counterpart country codes ISO3 -> ISO2
  dimnames(pip_side)$COUNTERPART_AREA <- ccode(
    dimnames(pip_side)$COUNTERPART_AREA, 'iso3c', 'iso2m',
    leaveifNA = TRUE, warn = FALSE)
  dimnames(pip_side)$COUNTERPART_AREA[
    dimnames(pip_side)$COUNTERPART_AREA == 'CN'] <- 'CN_X_HK'
  
  # Build renamed dim codes for re-ensuring after freq adjustment
  renamed_names <- c("INSTR", "REF_SECTOR", "COUNTERPART_SECTOR",
                     "COUNTERPART_AREA", "TIME")
  renamed_codes <- list(
    INSTR              = dimnames(pip_side)$INSTR,
    REF_SECTOR         = dimnames(pip_side)$REF_SECTOR,
    COUNTERPART_SECTOR = dimnames(pip_side)$COUNTERPART_SECTOR,
    COUNTERPART_AREA   = dimnames(pip_side)$COUNTERPART_AREA
  )
  
  # Frequency adjustment: A+S -> Q
  all_times    <- dimnames(pip_side)$TIME
  annual_times <- all_times[!grepl("s", all_times)]
  semi_times   <- all_times[grepl("s", all_times)]
  
  if (length(annual_times) > 0 & length(semi_times) > 0) {
    Apip <- subset_time(pip_side, annual_times, renamed_names, renamed_codes)
    Spip <- subset_time(pip_side, semi_times,   renamed_names, renamed_codes)
    frequency(Apip) <- 'Q'
    frequency(Spip) <- 'Q'
    Spip[onlyna = TRUE] <- Apip
    pip_side <- Spip
    rm(Apip, Spip)
  } else if (length(semi_times) > 0) {
    pip_side <- subset_time(pip_side, semi_times, renamed_names, renamed_codes)
    frequency(pip_side) <- 'Q'
  } else if (length(annual_times) > 0) {
    pip_side <- subset_time(pip_side, annual_times, renamed_names, renamed_codes)
    frequency(pip_side) <- 'Q'
  }
  
  # Filter from 2001q4 onwards
  pip_side <- subset_time(pip_side, "2001q4:", renamed_names, renamed_codes)
  
  # --- Compute sector aggregates (S12T, S12K, S12O, S1M) ---
  pip_side <- compute_sector_aggregates(pip_side)
  
  return(pip_side)
}


#############################################################################
# --- Helper: fill aa and ll for one country ---
# cc2 = ISO2 code for the reporting country
#############################################################################

fill_aa_ll <- function(pip_a, pip_l, cc2) {
  
  ###################################################################
  # FILL AA (assets) from ACCOUNTING_ENTRY = A
  ###################################################################
  if (!is.null(pip_a)) {
    aa[, cc2, , , "LE", "_P", , , usenames = TRUE, onlyna = TRUE] <<-
      aperm(pip_a, c(1, 2, 3, 5, 4))
  }
  
  ###################################################################
  # FILL LL (liabilities) from ACCOUNTING_ENTRY = L (direct)
  ###################################################################
  if (!is.null(pip_l)) {
    ll[, , , , "LE", "_P", , cc2, usenames = TRUE, onlyna = TRUE] <<-
      aperm(pip_l, c(1, 4, 3, 2, 5))
  }
  
  ###################################################################
  # FILL LL (liabilities) from ACCOUNTING_ENTRY = A (mirror)
  ###################################################################
  if (!is.null(pip_a)) {
    pip_mirror <- aperm(pip_a, c(1, 2, 3, 5, 4))
    nms_m <- names(dimnames(pip_mirror))
    nms_m[nms_m == "REF_SECTOR"]         <- "COUNTERPART_SECTOR_TEMP"
    nms_m[nms_m == "COUNTERPART_SECTOR"] <- "REF_SECTOR"
    nms_m[nms_m == "COUNTERPART_SECTOR_TEMP"] <- "COUNTERPART_SECTOR"
    nms_m[nms_m == "COUNTERPART_AREA"]   <- "REF_AREA"
    names(dimnames(pip_mirror)) <- nms_m
    ll[, cc2, , , "LE", "_P", , , usenames = TRUE, onlyna = TRUE] <<- pip_mirror
    rm(pip_mirror)
  }
  
  ###################################################################
  # Special S124 adjustment: F52 liabilities from F51 data
  ###################################################################
  if (!is.null(pip_l) &&
      "F51" %in% dimnames(pip_l)$INSTR &&
      "S124" %in% dimnames(pip_l)$REF_SECTOR) {
    
    ndim_l <- length(dim(pip_l))
    dots_after_2 <- paste(rep("", ndim_l - 2), collapse = ".")
    s124_slice <- pip_l["F51" %&% "." %&% "S124" %&% "." %&% dots_after_2]
    
    s124_names <- c("COUNTERPART_SECTOR", "COUNTERPART_AREA", "TIME")
    s124_codes <- list(
      COUNTERPART_SECTOR = dimnames(pip_l)$COUNTERPART_SECTOR,
      COUNTERPART_AREA   = dimnames(pip_l)$COUNTERPART_AREA
    )
    s124_slice <- ensure_dims(s124_slice, s124_names, s124_codes)
    
    ll["F52", , , "S124", "LE", "_P", , cc2, usenames = TRUE, onlyna = TRUE] <<-
      aperm(s124_slice, c(2, 1, 3))
    rm(s124_slice)
  }
}


#############################################################################
# --- EA AGGREGATE: data.table approach ---
# For each EA member:
#   1. Load cache, process through standard pipeline (MD3)
#   2. Convert processed MD3 to data.table (long format)
#   3. Exclude intra-EA counterpart rows
#   4. Append to accumulator data.table
#   5. Free MD3 object, gc()
# Then: sum by key, split by EA label, convert back to MD3, fill aa/ll.
#############################################################################

cat('=== Computing EA aggregate from member country PIP caches ===\n')

# Helper: extract year from quarterly time code
year_from_q <- function(tc) as.integer(sub("q[1-4]$", "", tc))

# Get all EA members that could ever appear
ea_all_members <- ea_members_for_year(2100)

# Scan available caches
ea_available <- character(0)
for (m in ea_all_members) {
  cachefile <- file.path(loaded_dir, 'pipbuffer/pip_' %&% m %&% '.rds')
  if (file.exists(cachefile)) ea_available <- c(ea_available, m)
}
cat('  EA members with caches:', length(ea_available), '/', length(ea_all_members), '\n')

if (length(ea_available) > 0) {
  
  # Accumulators: long-format data.tables for assets and liabilities
  dt_all_a <- list()
  dt_all_l <- list()
  
  for (m in ea_available) {
    cat('  ', m, '... ')
    cachefile <- file.path(loaded_dir, 'pipbuffer/pip_' %&% m %&% '.rds')
    pip_cc <- readRDS(cachefile)
    res <- process_pip_country(pip_cc)
    rm(pip_cc); gc()
    
    # --- Assets → data.table ---
    if (!is.null(res$pip_a)) {
      dt_a <- as.data.table(as.data.frame(res$pip_a))
      # as.data.frame on MD3 gives long format with dim columns + value column
      # Column names should be the dim names + "value"
      # Rename value column if needed
      valcol <- setdiff(names(dt_a), names(dimnames(res$pip_a)))
      if (length(valcol) == 1) setnames(dt_a, valcol, "value")
      dt_a <- dt_a[!is.na(value)]  # drop NAs immediately to save memory
      dt_a[, MEMBER := m]
      dt_all_a[[m]] <- dt_a
      rm(dt_a)
    }
    
    # --- Liabilities → data.table ---
    if (!is.null(res$pip_l)) {
      dt_l <- as.data.table(as.data.frame(res$pip_l))
      valcol <- setdiff(names(dt_l), names(dimnames(res$pip_l)))
      if (length(valcol) == 1) setnames(dt_l, valcol, "value")
      dt_l <- dt_l[!is.na(value)]
      dt_l[, MEMBER := m]
      dt_all_l[[m]] <- dt_l
      rm(dt_l)
    }
    
    rm(res); gc()
    cat('done\n')
  }
  
  # Combine all members
  cat('  Combining data.tables...\n')
  dt_a <- if (length(dt_all_a) > 0) rbindlist(dt_all_a) else NULL
  dt_l <- if (length(dt_all_l) > 0) rbindlist(dt_all_l) else NULL
  rm(dt_all_a, dt_all_l); gc()
  
  # Add year column for EA membership lookup
  if (!is.null(dt_a)) dt_a[, YEAR := year_from_q(TIME)]
  if (!is.null(dt_l)) dt_l[, YEAR := year_from_q(TIME)]
  
  # --- Exclude intra-EA rows ---
  # For each row, check if COUNTERPART_AREA is an EA member in that YEAR
  # Build a lookup: for each year, the set of EA ISO2 codes
  cat('  Excluding intra-EA positions...\n')
  
  # Pre-build a data.table of (YEAR, EA_ISO2) for fast filtering
  all_years_in_data <- sort(unique(c(
    if (!is.null(dt_a)) dt_a$YEAR else integer(0),
    if (!is.null(dt_l)) dt_l$YEAR else integer(0)
  )))
  
  ea_membership_dt <- rbindlist(lapply(all_years_in_data, function(yr) {
    data.table(YEAR = yr, EA_ISO2 = ea_members_iso2_for_year(yr))
  }))
  
  # Mark rows as intra-EA via a keyed join
  if (!is.null(dt_a)) {
    n_before <- nrow(dt_a)
    dt_a[, is_intra := FALSE]
    dt_a[ea_membership_dt, is_intra := TRUE,
         on = .(YEAR = YEAR, COUNTERPART_AREA = EA_ISO2)]
    dt_a <- dt_a[is_intra == FALSE]
    dt_a[, c("is_intra") := NULL]
    cat('    Assets: removed', n_before - nrow(dt_a), 'intra-EA rows,',
        nrow(dt_a), 'remaining\n')
  }
  
  if (!is.null(dt_l)) {
    n_before <- nrow(dt_l)
    dt_l[, is_intra := FALSE]
    dt_l[ea_membership_dt, is_intra := TRUE,
         on = .(YEAR = YEAR, COUNTERPART_AREA = EA_ISO2)]
    dt_l <- dt_l[is_intra == FALSE]
    dt_l[, c("is_intra") := NULL]
    cat('    Liabilities: removed', n_before - nrow(dt_l), 'intra-EA rows,',
        nrow(dt_l), 'remaining\n')
  }
  rm(ea_membership_dt); gc()
  
  # --- Add EA label per year ---
  if (!is.null(dt_a)) {
    dt_a[, EA_LABEL := sapply(YEAR, ea_label_for_year)]
  }
  if (!is.null(dt_l)) {
    dt_l[, EA_LABEL := sapply(YEAR, ea_label_for_year)]
  }
  
  # --- Also check EA membership: only sum members that belong to EA in that year ---
  # (e.g., HRV only from 2023, SVN only from 2007, etc.)
  cat('  Filtering by EA membership per year...\n')
  
  # Build (YEAR, MEMBER_ISO3) lookup
  ea_member_year_dt <- rbindlist(lapply(all_years_in_data, function(yr) {
    data.table(YEAR = yr, MEMBER = ea_members_for_year(yr))
  }))
  
  if (!is.null(dt_a)) {
    n_before <- nrow(dt_a)
    dt_a <- dt_a[ea_member_year_dt, on = .(YEAR, MEMBER), nomatch = NULL]
    cat('    Assets: kept', nrow(dt_a), 'rows (was', n_before, ')\n')
  }
  if (!is.null(dt_l)) {
    n_before <- nrow(dt_l)
    dt_l <- dt_l[ea_member_year_dt, on = .(YEAR, MEMBER), nomatch = NULL]
    cat('    Liabilities: kept', nrow(dt_l), 'rows (was', n_before, ')\n')
  }
  rm(ea_member_year_dt); gc()
  
  # --- Sum by key (across members), grouped by EA label ---
  key_cols <- c("INSTR", "REF_SECTOR", "COUNTERPART_SECTOR", "COUNTERPART_AREA", "TIME")
  
  # Process each EA label separately
  if (!is.null(dt_a)) ea_labels_a <- unique(dt_a$EA_LABEL) else ea_labels_a <- character(0)
  if (!is.null(dt_l)) ea_labels_l <- unique(dt_l$EA_LABEL) else ea_labels_l <- character(0)
  all_ea_labels <- sort(unique(c(ea_labels_a, ea_labels_l)))
  
  cat('  EA labels to fill:', paste(all_ea_labels, collapse = ', '), '\n')
  
  for (lbl in all_ea_labels) {
    cat('  Building MD3 for', lbl, '... ')
    
    # --- Assets ---
    ea_a_md3 <- NULL
    if (!is.null(dt_a) && lbl %in% dt_a$EA_LABEL) {
      dt_sub <- dt_a[EA_LABEL == lbl,
                     .(value = sum(value, na.rm = TRUE)),
                     by = key_cols]
      
      if (nrow(dt_sub) > 0) {
        dim_struct <- lapply(key_cols, function(col) sort(unique(dt_sub[[col]])))
        names(dim_struct) <- key_cols
        
        ea_a_md3 <- as.md3(
          dt_sub,
          id.vars = key_cols,
          dcstruct = dim_struct,
          timeid = "TIME",
          na.rm = TRUE
        )
      }
      rm(dt_sub)
    }
    
    # --- Liabilities ---
    ea_l_md3 <- NULL
    if (!is.null(dt_l) && lbl %in% dt_l$EA_LABEL) {
      dt_sub <- dt_l[EA_LABEL == lbl,
                     .(value = sum(value, na.rm = TRUE)),
                     by = key_cols]
      
      if (nrow(dt_sub) > 0) {
        dim_struct <- lapply(key_cols, function(col) sort(unique(dt_sub[[col]])))
        names(dim_struct) <- key_cols
        
        ea_l_md3 <- as.md3(
          dt_sub,
          id.vars = key_cols,
          dcstruct = dim_struct,
          timeid = "TIME",
          na.rm = TRUE
        )
      }
      rm(dt_sub)
    }
    
    # Fill aa/ll
    fill_aa_ll(ea_a_md3, ea_l_md3, lbl)
    rm(ea_a_md3, ea_l_md3); gc()
    cat('done\n')
  }
  
  rm(dt_a, dt_l); gc()
  cat('  EA aggregate: done\n')
  
} else {
  cat('  WARNING: No EA member caches found — skipping EA aggregate\n')
}


#############################################################################
# --- Main loop: process each country and fill aa, ll ---
#############################################################################

cat('\n=== Processing PIP data for', length(AREA), 'countries ===\n')

for (cc in AREA) {
  
  cachefile <- file.path(loaded_dir, 'pipbuffer/pip_' %&% cc %&% '.rds')
  if (!file.exists(cachefile)) { cat(cc, ': no cache file, skipping\n'); next }
  
  cat(as.character(Sys.time()), ': ', match(cc, AREA), '/', length(AREA),
      ' ', cc, '... ')
  
  pip_cc <- readRDS(cachefile)
  
  # Get ISO2 code for this country (for filling aa/ll)
  cc2 <- ccode(cc, 'iso3c', 'iso2m', leaveifNA = TRUE, warn = FALSE)
  if (cc2 == 'CN') cc2 <- 'CN_X_HK'
  
  # Process through shared pipeline
  res <- process_pip_country(pip_cc)
  rm(pip_cc); gc()
  
  # Fill aa/ll
  fill_aa_ll(res$pip_a, res$pip_l, cc2)
  
  rm(res)
  gc()
  cat('done\n')
}


#############################################################################
# --- Save outputs ---
#############################################################################

saveRDS(aa, file.path(data_dir, 'aa_iip_pip.rds'))
saveRDS(ll, file.path(data_dir, 'll_iip_pip.rds'))

saveRDS(aa, file.path(data_dir, 'vintages/aa_iip_pip_' %&% format(Sys.time(), '%F') %&% '_.rds'))
saveRDS(ll, file.path(data_dir, 'vintages/ll_iip_pip_' %&% format(Sys.time(), '%F') %&% '_.rds'))

cat('Done. Saved aa_iip_pip.rds and ll_iip_pip.rds\n')
