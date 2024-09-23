# Use this script to render all reports at once ################################

# After editing the dataset and optionally the location to move the
# rendered reports, click the `Source` button in RStudio.

# Read in data =================================================================

# EDIT: Replace `washi-data.csv` with the name of the same dataset used in
# 01_producer-report.qmd.
data <- read.csv(
  here::here("data/washi-data.csv"),
  check.names = FALSE,
  encoding = "UTF-8"
)

# Create a df with inputs for quarto::quarto_render() ==========================

# This creates a new df called `reports_html` with a row for every unique year
# and producer combo for the entire dataset.
#
# If you want a subset, filter the dataframe to include only the producers you
# want to create a report for.

## HTML reports
reports_html <- data |>
  dplyr::distinct(year, producer_id) |>
  dplyr::mutate(
    output_file = glue::glue("{year}_{producer_id}_Report.html"),
    output_format = "html",
    execute_params = purrr::map2(
      year,
      producer_id,
      \(year, producer_id) list(year = year, producer_id = producer_id)
    )
  ) |>
  dplyr::select(output_file, output_format, execute_params)

## Word docs
reports_docx <- reports_html |>
  dplyr::mutate(
    output_file = gsub("html", "docx", output_file),
    output_format = "docx"
  )

## Bind HTML and docx report dfs together
reports <- dplyr::bind_rows(reports_html, reports_docx)

# Render all reports to the project directory ==================================
reports |>
  purrr::pwalk(
    quarto::quarto_render,
    input = "01_producer-report.qmd",
    .progress = TRUE
  )

# Move rendered reports to a different directory ===============================

# Create directory called "reports"
output_dir <- fs::dir_create(here::here("reports"))

# List files with extensions html or docx.
files <- fs::dir_ls(here::here(), regexp = ".html$|.docx$")

# Move the files
fs::file_move(
  path = files,
  new_path = output_dir
)
