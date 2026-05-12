# Prepare your data
#
# Work through this script BEFORE rendering reports with render-reports.R.
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

# 2. Setup ---------------------------------------------------------------------

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

# Issues file: Excel file highlighting validation errors and warnings
issues_file <- "data/data-issues.xlsx"

# Output file: processed data used for report generation
output_file_rds <- "data/data-processed.rds"
output_file_xlsx <- "data/data-processed.xlsx"

# Language: are you using the English or Spanish template?
language <- "English"

# 3. Read data -----------------------------------------------------------------

input <- soils::read_soils_input(input_path)

if (length(input$issues) == 0) {
  cli::cli_alert_success("Successfully read data!")
} else {
  cli::cli_abort("Fix errors from {.fn read_soils_input} before continuing.")
}

# 4. Gate check ----------------------------------------------------------------

# Ensures required columns and basic structure are present.
# If this fails, downstream checks cannot run.

gate_result <- soils::check_input_structure(input)

if (length(gate_result$issues) == 0) {
  cli::cli_alert_success("Data passed gate check!")
} else {
  cli::cli_abort(
    "Fix errors from {.fn check_input_structure} before continuing."
  )
}

# 5. Validation ----------------------------------------------------------------

# Validate data and data dictionary. All errors must be corrected and warnings
# should be reviewed. After resolving errors, rerun steps 3-5 with your
# corrected data file(s).

# Run all validation checks
validation_result <- soils::run_all_checks(gate_result, language = language)

# Report validation results
if (length(validation_result$issues) > 0) {
  soils::format_issues(validation_result$issues)
  soils::create_issue_xlsx(
    validation_result,
    issues_file,
    language = language
  )
} else {
  cli::cli_alert_success("No issues to report!")
}

# Stop if there are errors present
if (isTRUE(validation_result$passed)) {
  cli::cli_alert_success("Data has no errors!")
} else {
  cli::cli_abort(
    "Fix errors from {.fn run_all_checks} before continuing."
  )
}

# 6. Process data --------------------------------------------------------------

# To run `process_data()`:
# - Errors must be resolved
# - Warnings may still exist but should have been reviewed

data_processed <- soils::process_data(validation_result, language = language)

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

# `data_processed` will be read into `01_producer-report.qmd`.

# Customize the .qmd files with your project-specific information and branding.

# See this tutorial for additional details on customizing the report content:

# fs::file_show(
#   "https://wa-department-of-agriculture.github.io/soils/articles/customize.html"
# )
