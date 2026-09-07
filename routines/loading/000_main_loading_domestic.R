################################################################################
# MAIN LOADING SCRIPT - Finflows - Domestic + Cross-Border
################################################################################

# Load required packages
if (!require("MDecfin")) {
  stop("MDecfin package is required but not installed")
}
# Set the project directories
if (!exists("data_dir")) data_dir <- '\\\\\\siprsto06p.delta.europa.eu/ECOFIN/FinFlows/githubrepo/data/loaded'
script_dir <- '\\\\\\siprsto06p.delta.europa.eu/ECOFIN/FinFlows/githubrepo/finflows/routines'

# Check directories exist
if (!dir.exists(script_dir)) {
  stop("Scripts directory not found: ", script_dir)
}
if (!dir.exists(data_dir)) {
  stop("Data directory not found: ", data_dir)
}

# Set working directory to script location
setwd(script_dir)

# Create logs directory if it doesn't exist
if (!dir.exists(file.path(data_dir, "logs"))) {
  dir.create(file.path(data_dir, "logs"))
}

cat("\nStarting execution of financial flows data loading...\n")
cat("Scripts directory:", script_dir, "\n")
cat("Data directory:", data_dir, "\n")

################################################################################
# Utility Functions
################################################################################

# Logging function
log_execution <- function(script_name, status, message = "") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s %s\n", timestamp, script_name, status, message)
  cat(log_entry, file = file.path(data_dir, "logs", "loading_log.txt"), append = TRUE)
  cat(log_entry) # Also print to console
}

# Script execution function
run_script <- function(script_path) {
  script_name <- basename(script_path)
  
  tryCatch({
    log_execution(script_name, "STARTING")
    start_time <- Sys.time()
    
    # Create environment with correct data path
    script_env <- new.env()
    script_env$data_dir <- data_dir
    
    # Source the script in the new environment
    sys.source(script_path, script_env)
    
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)
    
    log_execution(script_name, "COMPLETED", sprintf("(Duration: %s minutes)", duration))
    return(TRUE)
    
  }, error = function(e) {
    log_execution(script_name, "ERROR", sprintf("Error: %s", e$message))
    return(FALSE)
  })
}

# Interactive control function
confirm_data_load <- function(data_name) {
  if (!interactive()) {
    cat("Non-interactive session detected, proceeding with data load\n")
    return(TRUE)
  }
  
  cat("\n========================================\n")
  cat(sprintf("Do you want to load %s data?\n", data_name))
  cat("Type 'y' to load, or press Enter to skip: ")
  
  choice <- tolower(trimws(readline()))
  cat(sprintf("You chose to %s this data load\n", ifelse(choice == "y", "proceed with", "skip")))
  cat("========================================\n")
  return(choice == "y")
}

# Dependency checking functions
check_rds_exists <- function(file_path) {
  exists <- file.exists(file_path)
  if (exists) {
    log_execution("DEPENDENCY CHECK", "INFO", sprintf("Found existing file: %s", file_path))
  } else {
    log_execution("DEPENDENCY CHECK", "INFO", sprintf("File not found: %s", file_path))
  }
  return(exists)
}

get_file_modification_time <- function(file_path) {
  if (file.exists(file_path)) {
    return(file.info(file_path)$mtime)
  }
  return(NULL)
}

################################################################################
# Main Execution
################################################################################

# Initialize log file
cat("", file = file.path(data_dir, "logs", "loading_log.txt"))

# Define loading pipeline
loading_plan <- list(

  ##############################################################################
  # DOMESTIC DATA SOURCES
  ##############################################################################

  # Primary data sources - ECB QSA
  ecb_qsa = list(
    script = file.path(script_dir, "loading/001_finflowers_loadQSAfromECB.R"),
    output = file.path(data_dir, "fflist.rds"),
    name = "ECB QSA Financial Flows",
    phase = "Domestic"
  ),
  
  # Exchange rates data
  exchange_rates = list(
    script = file.path(script_dir, "loading/002_load_exchange_rates.R"),
    output = file.path(data_dir, "eurostat_f3_stocks.rds"),
    name = "Exchange Rates Data",
    phase = "Domestic"
  ),
  
  # Eurostat NASA data
  nasa_data = list(
    script = file.path(script_dir, "loading/003_load_NASA.R"),
    output = file.path(data_dir, "domestic_loading_data_files/nasa_unconsolidated_stocks_assets.rds"),
    name = "Eurostat NASA Annual Data",
    phase = "Domestic"
  ),
  
  # Eurostat NASQ data
  nasq_data = list(
    script = file.path(script_dir, "loading/004_load_NASQ.R"),
    output = file.path(data_dir, "domestic_loading_data_files/nasq_S.rds"),
    name = "Eurostat NASQ Quarterly Data",
    phase = "Domestic"
  ),
  
  # ECB BSI MFI data
  bsi_mfi = list(
    script = file.path(script_dir, "loading/005_load_BSI_MFI.R"),
    output = file.path(data_dir, "bsi_assets.rds"),
    name = "ECB BSI MFI Holdings",
    phase = "Domestic"
  ),
  
  # ECB BSI Loans and Deposits
  bsi_loans_deposits = list(
    script = file.path(script_dir, "loading/006_load_bsi_loans_dep.R"),
    output = file.path(data_dir, "bsi_loans_dep.rds"),
    name = "ECB BSI Loans and Deposits",
    phase = "Domestic"
  ),
  
  # Counterpart information
  counterpart_info = list(
    script = file.path(script_dir, "loading/007_load_counterpart_info.R"),
    output = file.path(data_dir, "cpq_new.rds"),
    name = "Counterpart Information (NASA CP)",
    phase = "Domestic"
  ),
  
  # Securities Holdings Statistics
  shs_data = list(
    script = file.path(script_dir, "loading/008_load_shs_s.R"),
    output = file.path(data_dir, "ash.rds"),
    name = "ECB Securities Holdings Statistics",
    phase = "Domestic"
  ),
  
  # Row equity processing
  row_equity = list(
    script = file.path(script_dir, "loading/009_row_equity_processing.R"),
    output = file.path(data_dir, "af51.rds"),
    name = "Rest of World Equity Processing",
    phase = "Domestic"
  ),
  
  # Government F5 data
  gov_f5 = list(
    script = file.path(script_dir, "loading/010_govF5.R"),
    output = file.path(data_dir, "gov_equity.rds"),
    name = "Government F5 Holdings",
    phase = "Domestic"
  ),
  
  # ECB Central Bank Holdings
  ecb_capital = list(
    script = file.path(script_dir, "loading/011_central_bank_holdings_ECB_F519.R"),
    output = file.path(data_dir, "ecb_capital_md3.rds"),
    name = "ECB Central Bank Capital Holdings",
    phase = "Domestic"
  ),

  ##############################################################################
  # CROSS-BORDER DATA SOURCES
  ##############################################################################

  # F81, F89, F6 data
  f81_f89_f6 = list(
    script = file.path(script_dir, "loading/012_loading_F81_F89_F6.R"),
    output = file.path(data_dir, "f81_f89_f6.rds"),
    name = "F81, F89, F6 Data",
    phase = "Cross-Border"
  ),

  # IIP / BOP data from Eurostat (external sector positions)
  iip_bop = list(
    script = file.path(script_dir, "loading/013_load_iip_bop.R"),
    output = file.path(data_dir, "iip_cps.rds"),
    name = "Eurostat IIP/BOP External Positions",
    phase = "Cross-Border"
  ),
  
  # PIP data from IMF (bilateral portfolio positions)
  pip = list(
    script = file.path(script_dir, "loading/014_load_pip.R"),
    output = file.path(data_dir, "pipbuffer/allpipresults.rds"),
    name = "IMF Portfolio Investment Positions",
    phase = "Cross-Border"
  ),
  
  # FDI data from OECD (bilateral direct investment positions)
  fdi = list(
    script = file.path(script_dir, "loading/015_load_fdi.R"),
    output = file.path(data_dir, "fdibuffer/allfdi_pos_ctry.rds"),
    name = "OECD Foreign Direct Investment Positions",
    phase = "Cross-Border"
  ),
  
  # Banking sector data from BIS (bilateral F, F2, and F4 positions)
  lbs = list(
    script = file.path(script_dir, "loading/016_load_lbs.R"),
    output = file.path(data_dir, "lbsbuffer/alllbs.rds"),
    name = "BIS Locational Banking Statistics",
    phase = "Cross-Border"
  )
  
)

# Execute data loading pipeline
log_execution("MAIN", "INFO", "Starting data loading phase")

current_phase <- ""
for (data_source_name in names(loading_plan)) {
  data_source <- loading_plan[[data_source_name]]
  
  # Log phase changes
  if (!is.null(data_source$phase) && data_source$phase != current_phase) {
    current_phase <- data_source$phase
    log_execution("PHASE", "INFO", sprintf("Starting %s loading", current_phase))
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat("PHASE:", current_phase, "loading\n")
    cat(paste(rep("-", 60), collapse = ""), "\n")
  }
  
  if (confirm_data_load(data_source$name)) {
    if (!file.exists(data_source$script)) {
      stop(sprintf("Script file not found: %s", data_source$script))
    }
    
    success <- run_script(data_source$script)
    if (!success) {
      stop(sprintf("Execution failed at script: %s", data_source$script))
    }
    
    # Verify output was created
    if (!is.null(data_source$output) && !file.exists(data_source$output)) {
      log_execution(data_source_name, "WARNING", sprintf("Expected output not found: %s", data_source$output))
    }
    
  } else {
    log_execution(data_source$name, "SKIPPED", "User chose to skip loading")
  }
}

# Final loading summary
log_execution("MAIN", "COMPLETED", "All data loading operations completed")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("DATA LOADING SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Loading phase completed successfully!\n")
cat("Check the logs directory for detailed execution logs.\n")
cat("\nNext step: Run domestic/000_main_filling.R to process and fill the loaded data.\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

################################################################################
# End of Loading Script
################################################################################
