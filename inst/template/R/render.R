#' Render word docs
#'
#' After rendering to word, open files and make sure formatting looks good.
#' Flextables may need to be adjusted if there are many columns.
#'
#' @param producerId Character producerId to render report for.
#' @param year Year of samples to include in report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' unique(exampleData$producerId) |>
#'   as.character() |>
#'   purrr::walk(\(x) render_to_html(x, 2022),
#'     .progress = TRUE
#'   )
#' }
render_to_docx <- function(producerId, year) {
  quarto::quarto_render(
    input = paste0(
      here::here(),
      "/inst/", year, "_producer_report.qmd"
    ),
    output_format = "docx",
    output_file = paste0(
      year, "_", producerId, ".docx"
    ),
    execute_params = list(
      producerId = producerId,
      year = year
    )
  )
}

#' Render HTML reports
#'
#' Unfortunately, self-contained html reports must be rendered to the same folder
#' as the .qmd html reports should be manually moved to the inst/reports/
#' subfolder after rendering
#' https://stackoverflow.com/questions/72346829/two-problems-rendering-a-qmd-file-with-quarto-render-from-r
#'
#' @param producerId Character producerId to render report for.
#' @param year Year of samples to include in report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' unique(exampleData$producerId) |>
#'   as.character() |>
#'   purrr::walk(\(x) render_to_html(x, 2022),
#'     .progress = TRUE
#'   )
#' }
render_to_html <- function(producerId, year) {
  quarto::quarto_render(
    input = paste0(
      here::here(),
      "/inst/", year, "_producer_report.qmd"
    ),
    output_format = "html",
    output_file = paste0(
      year, "_", producerId, ".html"
    ),
    execute_params = list(
      producerId = producerId,
      year = year
    )
  )
}
