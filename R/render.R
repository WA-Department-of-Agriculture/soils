#' Render producer reports
#'
#' Render `_producerReport.qmd` to .html or .docx files. The word document
#' outputs should be opened and manually edited to ensure the report looks
#' good. See `vignette("docx", package = "soils")` for more details on these
#' edits
#'
#' @param producerId Character producerId to render report for.
#' @param year Year of samples to include in report.
#' @param output Target output format.
#'
#'   Currently supported options:
#'
#'   * 'html' for an interactive report.
#'   * 'docx' for an editable word document.
#'   * 'all' for rendering all supported output formats.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Render single html report for first producer in data set
#' first_producer <- head(exampleData$producerId, 1)
#' render_report(
#'   first_producer,
#'   year = 2023,
#'   output = "html"
#' )
#'
#' # Render docx reports for all 2023 producers
#' unique(exampleData$producerId) |>
#'   purrr::walk(
#'     \(producerId) render_report(
#'       producerId,
#'       year = 2023,
#'       output = "docx",
#'       .progress = TRUE
#'     )
#'   )
#' }
render_report <- function(producerId, year, output = "html") {
  rlang::arg_match(
    output,
    values = c("docx", "html", "all")
  )

  quarto::quarto_render(
    input = paste0(
      here::here(),
      "/inst/",
      "_producerReport.qmd"
    ),
    output_format = output,
    output_file = paste0(
      year,
      "_",
      producerId,
      "report"
    ),
    execute_params = list(
      producerId = producerId,
      year = year
    )
  )

  # Move files from main project folder to reports folder
  files <- list.files(
    path = here::here(),
    pattern = "\\report.docx$|\\report.html$",
    full.names = FALSE
  )

  fs::file_move(
    path = paste0(here::here(), "/", files),
    new_path = paste0(here::here(), "/inst/reports/", files)
  )
}
