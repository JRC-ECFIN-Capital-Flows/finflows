# Load required packages
library(data.table)
library(openxlsx)
library(MD3)

# Set data directory
if (!exists("data_dir")) data_dir = getwd()

# ECB Capital Processing Script
# 1. Import the Excel file
ecb_file_path <- file.path(data_dir, "static/ecb_capital.xlsx")

# Read annual paid-up capital (sheet ECB_capital, EUR millions)
# Header row is on row 4: Year | Paid-up capital (EUR mn) | Paid-up capital (EUR bn) | Note
capital_ts <- as.data.table(read.xlsx(ecb_file_path, sheet = "ECB_capital", startRow = 4))
# Keep only Year and the EUR-mn column, rename for clarity
capital_ts <- capital_ts[, .(Year = as.integer(Year),
                             paid_up_eur_mn = as.numeric(`Paid-up.capital.(EUR.mn)`))]
# Drop rows where Year is NA (footnote rows after the data)
capital_ts <- capital_ts[!is.na(Year)]
years <- capital_ts$Year

# Read capital keys (sheet Capital_keys_long: long format, all regimes)
# Columns: Effective_date_label | Effective_date | ISO | Country | Group | Capital_key_pct | Note
capital_keys_long <- as.data.table(read.xlsx(ecb_file_path, sheet = "Capital_keys_long", startRow = 1))
capital_keys_long[, Capital_key_pct := as.numeric(Capital_key_pct)]
# The Effective_date_label is what we will use to identify the regime; build a short tag
# matching the year of effect for readability ("1999", "2007", "2009", "2014", "2019", "2020", "2024")
capital_keys_long[, regime_tag := substr(Effective_date, 1, 4)]
# 1 Feb 2020 still has tag "2020" (good); 1 Jul 2013 also gets tag "2013" but is unused below

# 2. Extract total ECB capital values for each year (EUR millions)
ecb_total <- capital_ts$paid_up_eur_mn
names(ecb_total) <- as.character(capital_ts$Year)

print("ECB Total Paid-up Capital by Year (EUR mn):")
print(ecb_total)

print("Capital Keys (long format) - first rows:")
print(head(capital_keys_long))

# 3. Create quarterly time series with country participation
#    Following original-script logic: each regime applies for full calendar quarters of certain years.
#    Mapping decisions:
#      - 1999 keys cover 1999q4 - 2003q4
#      - 2007 keys cover 2004q1 - 2008q4 (extended back to fill missing 2004/2007 regime gap)
#      - 2009 keys cover 2009q1 - 2013q4 (1 Jul 2013 HR-accession keys not used)
#      - 2014 keys cover 2014q1 - 2018q4
#      - 2019 keys cover 2019q1 - 2020q1
#      - 2020 keys cover 2020q2 - 2023q4 (Brexit; skip partial 2020q1)
#      - 2024 keys cover 2024q1 - 2025q4 (extended forward; 2026 BG accession excluded as out of range)
time_periods <- list(
  "1999" = c("1999q4", paste0(rep(2000:2003, each = 4), "q", rep(1:4, 4))),
  "2007" = paste0(rep(2004:2008, each = 4), "q", rep(1:4, 5)),
  "2009" = paste0(rep(2009:2013, each = 4), "q", rep(1:4, 5)),
  "2014" = paste0(rep(2014:2018, each = 4), "q", rep(1:4, 5)),
  "2019" = c(paste0("2019q", 1:4), "2020q1"),
  "2020" = c(paste0("2020q", 2:4), paste0(rep(2021:2023, each = 4), "q", rep(1:4, 3))),
  "2024" = paste0(rep(2024:2025, each = 4), "q", rep(1:4, 2))
)

# Sanity check: every regime tag used here must exist in the keys file
stopifnot(all(names(time_periods) %in% unique(capital_keys_long$regime_tag)))

# Create complete quarterly sequence (1999q4 .. 2025q4)
all_quarters <- c()
for (year in 1999:2025) {
  if (year == 1999) {
    all_quarters <- c(all_quarters, "1999q4")
  } else {
    all_quarters <- c(all_quarters, paste0(year, "q", 1:4))
  }
}

# Non-EA paid-up percentage by date (5% pre-2004; 7% from 2004 to 28 Dec 2010; 3.75% from 29 Dec 2010)
# Using year-aligned logic: the 7% regime starts 2004q1 and the 3.75% regime starts 2011q1
nea_paid_pct_for_quarter <- function(quarter) {
  year <- as.integer(substr(quarter, 1, 4))
  if (year <= 2003)               return(0.05)
  if (year >= 2004 && year <= 2010) return(0.07)
  return(0.0375)
}

# Initialise result data.table
result_dt <- data.table()

# Process each (regime, country) combination
for (rg in names(time_periods)) {
  quarters_in_regime <- time_periods[[rg]]
  keys_in_regime     <- capital_keys_long[regime_tag == rg]
  
  if (nrow(keys_in_regime) == 0) {
    warning(sprintf("No keys found for regime '%s' - skipping", rg))
    next
  }
  
  for (i in 1:nrow(keys_in_regime)) {
    country     <- keys_in_regime[i, ISO]
    capital_key <- keys_in_regime[i, Capital_key_pct]
    group       <- keys_in_regime[i, Group]   # "EA" or "NEA"
    
    # Skip the United Kingdom from 2020q2 onward (no longer in ESCB after 1 Feb 2020).
    # In the 2019 regime UK is still present, so it gets values for 2019q1..2020q1; from 2020q2
    # onward UK no longer appears in capital_keys_long, so the inner loop naturally skips it.
    
    country_series <- data.table(
      REF_AREA  = country,
      TIME      = quarters_in_regime,
      obs_value = numeric(length(quarters_in_regime)),
      group     = group
    )
    
    for (quarter in quarters_in_regime) {
      year <- as.character(as.integer(substr(quarter, 1, 4)))
      
      # Subscribed capital by year (EUR mn) - constant within each subscribed-capital-regime:
      #   €5 bn at inception; €5.565 bn after 2004 enlargement; €5.761 bn after 2007 enlargement;
      #   €10.761 bn from 29 Dec 2010 capital increase; €10.825 bn after Croatia's 2013 EU accession
      subscribed <- if      (year <= "2003") 5000
      else if (year <= "2006") 5564.669
      else if (year <= "2009") 5760.652
      else if (year <= "2013") 10760.652
      else                     10825.007
      
      if (group == "EA") {
        paid_pct <- 1
      } else {
        paid_pct <- nea_paid_pct_for_quarter(quarter)
      }
      
      country_series[TIME == quarter,
                     obs_value := capital_key * subscribed * paid_pct]
    }
    
    result_dt <- rbind(result_dt, country_series)
  }
}

# 3b. Brexit transition adjustment (2020q2 - 2022q4)
#     Background: on 1 Feb 2020 the UK left the ESCB. Its subscribed capital share was
#     reallocated immediately to the remaining 27 NCBs, but the additional paid-up amount was
#     phased in by EA NCBs over a two-year transition: instalments at end-2020 and end-2021.
#     The naive "key * subscribed * paid_pct" calculation therefore overstates EA NCBs' actual
#     paid-up capital during 2020q2-2022q4 (it assumes the new full subscription is paid up
#     immediately).
#
#     Correction (Option 2 - EA-only scaling):
#       * Keep non-EA values unchanged (they already reflect the 3.75% rate).
#       * Scale EA NCB values within each year so that:
#           sum_EA(scaled)  =  published_total[year]  -  sum_NEA(unchanged)
#         which preserves each EA NCB's relative share among EA NCBs but scales their level
#         to match the actual phased-in paid-up amount.

transition_quarters <- c(paste0("2020q", 2:4),
                         paste0("2021q", 1:4),
                         paste0("2022q", 1:4))

for (q in transition_quarters) {
  year      <- as.character(as.integer(substr(q, 1, 4)))
  published <- ecb_total[year]
  if (is.na(published)) next
  
  ea_sum_raw  <- result_dt[TIME == q & group == "EA",  sum(obs_value, na.rm = TRUE)]
  nea_sum_raw <- result_dt[TIME == q & group == "NEA", sum(obs_value, na.rm = TRUE)]
  
  if (ea_sum_raw <= 0) next
  
  ea_target   <- published - nea_sum_raw
  ea_scale    <- ea_target / ea_sum_raw
  
  result_dt[TIME == q & group == "EA",
            obs_value := obs_value * ea_scale]
}

print("Sample of result data:")
print(head(result_dt, 20))

print("Summary by country (EUR mn):")
print(result_dt[, .(min_value  = min(obs_value, na.rm = TRUE),
                    max_value  = max(obs_value, na.rm = TRUE),
                    mean_value = mean(obs_value, na.rm = TRUE),
                    n_quarters = .N), by = REF_AREA])

# Cross-check: sum across countries at a few selected dates should approximately match the
# published total paid-up capital from the ECB_capital sheet
print("Cross-check: sum across all NCBs vs published total (EUR mn):")
check_quarters <- c("2009q1", "2014q1", "2019q1", "2020q2", "2021q1", "2022q4", "2024q1", "2025q4")
for (q in check_quarters) {
  yr <- as.character(as.integer(substr(q, 1, 4)))
  observed_sum <- result_dt[TIME == q, sum(obs_value, na.rm = TRUE)]
  published    <- ecb_total[yr]
  print(sprintf("  %s: computed = %.1f, published = %.1f, diff = %.2f",
                q, observed_sum, published, observed_sum - published))
}

# 4. Convert to MD3 object
# Drop the helper 'group' column before the MD3 conversion
result_dt[, group := NULL]

# Ensure proper data types
result_dt[, REF_AREA  := as.character(REF_AREA)]
result_dt[, TIME      := as.character(TIME)]
result_dt[, obs_value := as.numeric(obs_value)]

# Convert to MD3
ecb_capital_md3 <- as.md3(result_dt)

print("MD3 Structure:")
print(str(ecb_capital_md3))

print("Sample MD3 values (Austria, 2023):")
print(ecb_capital_md3["AT", "2023q1:2023q4"])

# Save results
saveRDS(ecb_capital_md3, file.path(data_dir, "ecb_capital_md3.rds"))
write.csv(result_dt, file.path(data_dir, "ecb_capital_quarterly.csv"), row.names = FALSE)

print("Processing complete!")
print(paste("Quarters processed:", length(all_quarters)))
print(paste("Country-quarter rows:", nrow(result_dt)))
print(paste("Unique countries:", length(unique(result_dt$REF_AREA))))
