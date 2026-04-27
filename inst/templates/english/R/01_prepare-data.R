# Prepare your data
#
# Work through this script BEFORE rendering reports.
#
# This will:
# 1. Open the example data and data dictionary for your reference
# 2. Setup file paths (this is the only section in this script to edit)
# 3. Load your data
# 4. Gate check that the file structure is valid
# 5. Validate your data and report any issues
# 6. Process your data
# 7. Save your processed data to be read into 01_producer-report.qmd for reports

# 1. Download example data -----------------------------------------------------

# Uncomment to open a template spreadsheet with example data and a data
# dictionary. Replace the example data with your own.

# fs::file_show("data/template.xlsx")

# 2. EDIT: Setup ---------------------------------------------------------------

# Provide the path to your input data.
#
# You can use ONE of the following formats:
#
# a. One Excel (.xlsx) file with two sheets:
#    - "Data"
#    - "Data Dictionary"
#
# b) Two .csv files:
#    - First = Data
#    - Second = Data Dictionary
#
input_path <- c(
  "tests/test-files/example-data-dupes.csv",
  "tests/test-files/example-data-dictionary-dupes.csv"
)
input_path <- "tests/test-files/invalid-measurement-groups.xlsx"

# Output file: processed data used for report generation
output_file <- "tests/test-files/processed-data.rds"

# Issues file: Excel file highlighting validation errors and warnings
issues_file <- "tests/test-files/data-issues.xlsx"

# Language to render report: "english" or "spanish"
language <- "english"

# 3. Load data -----------------------------------------------------------------

input <- read_soils_input(input_path)

# 4. Gate check ----------------------------------------------------------------

# Ensures required columns and basic structure are present.
# If this fails, downstream checks cannot run.

gate_result <- check_input_structure(input)

# 5. Validation ----------------------------------------------------------------

# Run all validation checks
issues <- run_all_checks(gate_result)

# Print issues to console
if (length(issues) > 0) {
  format_output(issues)
} else {
  cli::cli_alert_success("Data successfully validated!")
}

# Create spreadsheet with issues and conditional formatting
if (length(issues) > 0) {
  create_issue_xlsx(gate_result, issues_file, issues, language)
} else {
  cli::cli_alert_success("No issues to report!")
}

# 6. Process data --------------------------------------------------------------

# At this point:
# - Errors have been resolved
# - Warnings may still exist but are non-blocking

process_data()

# 7. Save processed data -------------------------------------------------------

saveRDS()
writexl::write_xlsx()

cli::cli_alert_success(
  "Processed data saved as {.file data/processed-data.RDS}"
)
