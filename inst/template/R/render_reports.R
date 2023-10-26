# Use this script to render all reports at once #######################

# Read in data --------------------------------------------------------------

# EDIT: Read in the same dataset used in producer_report.qmd.
data <- read.csv(
  paste0(here::here(), "/data/exampleData.csv"),
  check.names = FALSE,
  encoding = "UTF-8"
)

# Create a df with inputs for quarto::quarto_render() -----------------------

## HTML reports
reports_html <- data |>
  dplyr::distinct(year, producerId) |>
  dplyr::mutate(
    output_file = glue::glue("{year}_{producerId}_Report.html"),
    output_format = "html",
    execute_params = purrr::map2(
      year,
      producerId,
      \(year, producerId) list(year = year, producerId = producerId)
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
reports <- rbind(reports_html, reports_docx)

# Render all reports to the project directory -----------------------
reports_html |>
  purrr::pwalk(
    quarto::quarto_render,
    input = "01_producer_report.qmd",
    .progress = TRUE
  )

# Move rendered reports to a different directory ----------------------------

# OPTIONAL EDIT: set out_dir to where you want the reports moved to

output_dir <- paste0(here::here(), "/reports")

# List files with names that end in _Report and have docx or html extensions.
files <- list.files(
  path = here::here(),
  pattern = "\\_Report.docx$|\\_Report.html$",
  full.names = FALSE
)

# Create directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Move the files
fs::file_move(
  path = paste0(here::here(), "/", files),
  new_path = paste0(output_dir, "/", files)
)
