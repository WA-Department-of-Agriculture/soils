#' Render producer reports
#'
#' Render `_producerReport.qmd` to `.html` or `.docx` files. The word document
#' outputs should be opened and manually edited to ensure the report looks good.
#' See `vignette("docx", package = "soils")` for more details on these edits
#'
#' @param producerId Character `producerId` to render report for.
#' @param year Year of samples to include in report.
#' @param output Target output format.
#'
#'   Currently supported options:
#'
#'   * `"html"` for an interactive report.
#'   * `"docx"` for an editable word document.
#'   * `"all"` for rendering all supported output formats.
#' @param output_dir Path to the directory that holds the reports. If `NULL`,
#'   the reports will be saved in the project directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Render single html report for first producer in data set
#' first_producer <- head(exampleData$producerId, 1)
#' render_producer_report(
#'   first_producer,
#'   year = 2023,
#'   output = "html",
#'   output_dir = paste0(here::here(),"/inst/reports/")
#' )
#'
#' # Render docx reports for all 2023 producers
#' unique(exampleData$producerId) |>
#'   purrr::walk(
#'     \(producerId) render_producer_report(
#'       producerId,
#'       year = 2023,
#'       output = "docx",
#'       output_dir = paste0(here::here(),"/inst/reports/"),
#'       .progress = TRUE
#'     )
#'   )
#' }
render_producer_report <- function(producerId,
                                   year,
                                   output = "html",
                                   output_dir = NULL
                                   ) {
  rlang::arg_match(
    output,
    values = c("docx", "html")
  )

  if (is.null(output_dir)) {
    output_dir <- here::here()
  }

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
      "_Report.",
      output
    ),
    execute_params = list(
      producerId = producerId,
      year = year
    )
  )

  # Move files from main project folder to reports folder
  files <- list.files(
    path = here::here(),
    pattern = "\\_Report.docx$|\\_Report.html$",
    full.names = FALSE
  )

  # Create directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  fs::file_move(
    path = paste0(here::here(), "/", files),
    new_path = paste0(output_dir, "/", files)
  )
}
