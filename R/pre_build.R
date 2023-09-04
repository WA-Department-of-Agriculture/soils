# This should automatically be run during the package build process

template_path <- paste0(here::here(), "/inst/template")

# Copy R files to template folder
files <- c(
  "helpers.R",
  "map.R",
  "plots.R",
  "render.R",
  "tables.R"
)

fs::file_copy(
  path = paste0(here::here(), "/R/", files),
  new_path = paste0(template_path, "/R/", files),
  overwrite = TRUE
)

# Remove intermediate files from testing template
delete <- c(
  "producer_report.html",
  "producer_report.docx",
  "producer_report_files",
  "figure_output",
  "reports"
)

purrr::walk(delete, \(file) {
  path <- paste0(template_path, "/", file)
  if (fs::file_exists(path)) {
    fs::file_delete(
      path = path
    )
  }
})
