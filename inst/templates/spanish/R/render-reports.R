# Render all reports at once

# Read processed data that was created from prepare-data.R
data <- readRDS("data/data-processed.rds")
data <- input$results_wide

# Optional: Filter data to create reports for only a subset of producers
# data <- data |>
#   dplyr::filter(year %in% c(2024) & producer_id %in% c("P001", "P002"))

# Render all reports
soils::render_reports(
  data = data,
  # Formats of reports - interactive HTML and/or static MS Word docx
  formats = c("html", "docx"),
  # Directory where rendered reports should be moved.
  output_dir = "reports"
)
