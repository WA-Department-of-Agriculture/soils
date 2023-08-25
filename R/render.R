#' Render word docs
#'
#' @param producerId Character producerId to render report for.
#' @param year Year of samples to include in report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' unique(example_data_wide$producerId) |>
#'   as.character() |>
#'   purrr::walk(\(x) render_to_docx(x, 2022))
#' }
render_to_docx <- function(producerId, year) {
  quarto::quarto_render(
    input = paste0(
      here::here(),
      "/inst/producer_report.qmd"
    ),
    output_format = "docx",
    output_file = paste0(
      producerId, ".docx"
    ),
    execute_params = list(
      producerId = producerId,
      year = year
    )
  )
}

# if no changes to individual reports are desired, open Adobe Acrobat > Create
# PDF > Multiple Files > Create Multiple PDF Files > Uncheck "Overwrite file",
# add all word files then click okay!

# render html reports
#
# unfortunately, self-contained html reports must be rendered to the same folder
# as the .qmd html reports should be manually moved to the qmd/reports/
# subfolder after rendering
# https://stackoverflow.com/questions/72346829/two-problems-rendering-a-qmd-file-with-quarto-render-from-r

# https://github.com/quarto-dev/quarto-cli/discussions/2171 has a function to
# copy the files to another folder then delete them from the parent folder

#' Render .qmd to html
#'
#' @param producerId Character producerId to render report for.
#' @param year Year of samples to include in report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' unique(example_data_wide$producerId) |>
#'   as.character() |>
#'   purrr::walk(\(x) render_to_html(x, 2022))
#' }
render_to_html <- function(producerId, year) {
  quarto::quarto_render(
    input = paste0(
      here::here(),
      "/inst/producer_report.qmd"
    ),
    output_format = "html",
    output_file = paste0(
      producerId, ".html"
    ),
    execute_params = list(
      producerId = producerId,
      year = year
    )
  )
}
