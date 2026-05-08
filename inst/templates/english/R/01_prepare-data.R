# Prepare your data
#
# Work through this script BEFORE rendering reports.
#
# This will:
# 1. Open the example data and data dictionary for your reference
# 2. Setup file paths (`input_path` is the only line you need to edit)
# 3. Load your data
# 4. Gate check that the file structure is valid
# 5. Validate your data and report any issues
# 6. Process your data
# 7. Save your processed data to be read into 01_producer-report.qmd for reports

# 1. Download example data -----------------------------------------------------

# Uncomment to open a template spreadsheet with example data and a data
# dictionary. Replace the example data with your own.

# fs::file_show("data/template.xlsx")

# 2. Setup ---------------------------------------------------------------

# EDIT: Provide the path to your input data.
#
# You can use ONE of the following formats:
#
# a. One Excel (.xlsx) file with two sheets:
#    - "Data"
#    - "Data Dictionary"
#
#    Example:
#    input_path <- "data/data-and-dictionary.xlsx"
#
# b) Two .csv files as a character vector:
#    - First = "data.csv"
#    - Second = "data-dictionary.csv"
#
#    Example:
#    input_path <- c("data/data.csv", "data/data-dictionary.csv")

input_path <- c(
  "data/template.xlsx"
)

# Do not change the below file paths.

# Output file: processed data used for report generation
output_file_rds <- "data/data-processed.rds"
output_file_xlsx <- "data/data-processed.xlsx"

# Issues file: Excel file highlighting validation errors and warnings
issues_file <- "data/data-issues.xlsx"

# Language: are you using the English or Spanish template?
language <- "English"

# 3. Read data -----------------------------------------------------------------

input <- read_soils_input(input_path)

# 4. Gate check ----------------------------------------------------------------

# Ensures required columns and basic structure are present.
# If this fails, downstream checks cannot run.

gate_result <- check_input_structure(input)

# 5. Validation ----------------------------------------------------------------

# Validate data and data dictionary. All errors must be corrected and warnings
# should be reviewed. After resolving errors, repeat steps 3-5 with your
# corrected data file(s).

# Run all validation checks
issues <- run_all_checks(gate_result, language = language)

# Check for issues
has_issues <- length(issues) > 0
has_errors <- purrr::some(issues, ~ identical(.x$severity, "error"))
has_warnings <- purrr::some(issues, ~ identical(.x$severity, "warning"))

# Status messaging
if (has_errors && has_warnings) {
  cli::cli_alert_danger("Validation failed with errors and warnings.")
} else if (has_errors) {
  cli::cli_alert_danger("Validation failed with errors")
} else if (has_warnings) {
  cli::cli_alert_warning("Validation passed with warnings.")
} else {
  cli::cli_alert_success("Data successfully validated without issues!")
}

# If issues exist, print to console
if (has_issues) {
  format_output(issues)
}

# If issues exist, create spreadsheet with conditional formatting
if (has_issues) {
  create_issue_xlsx(gate_result, issues_file, issues, language = language)
} else {
  cli::cli_alert_success("No issues to report!")
}

# Add `passed` flag (TRUE if no errors)
gate_result$passed <- !has_errors

# 6. Process data --------------------------------------------------------------

# To run `process_data()`:
# - Errors must be resolved
# - Warnings may still exist but should have been reviewed

data_processed <- process_data(gate_result, language = language)

# 7. Save processed data -------------------------------------------------------

saveRDS(data_processed, output_file_rds)
writexl::write_xlsx(data_processed, output_file_xlsx)

cli::cli_inform(
  c(
    "v" = "Processed data saved as:",
    "*" = "{.file {output_file_rds}}",
    "*" = "{.file {output_file_xlsx}}"
  )
)

# 8. Customize the report .qmd files -------------------------------------------

# The `data_processed` will be read into `01_producer-report.qmd`. Customize the
# .qmd files with your project-specific information and branding.

# See this tutorial for additional details on customizing the report content:

# fs::file_show(
#   "https://wa-department-of-agriculture.github.io/soils/articles/customize.html"
# )
