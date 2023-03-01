# render word docs

#' render_docx
#'
#' @param producerId
#'
#' @export
#'
#' @examples
#' unique(results22$producerId) |>
#' as.character() |>
#' walk(render_docx)

render_docx <- function(producerId) {
  box::use(here[here],
           glue[glue],
           quarto[quarto_render])
  quarto_render(
    input = paste0(
      here(),
      "/qmd/producer_report.qmd"
    ),
    output_format = "docx",
    output_file = glue("qmd/reports/{producerId}.docx"),
    execute_params = list(
      producerId = producerId,
      year = params$year
    )
  )
}

# if no changes to individual reports are desired, open
# Adobe Acrobat > Create PDF > Multiple Files >
# Create Multiple PDF Files > Uncheck "Overwrite file",
# add all word files then click okay!

# render html reports
#
# unfortunately, self-contained html reports must be rendered
# to the same folder as the .qmd html reports should be manually
# moved to the qmd/reports/ subfolder after rendering
# https://stackoverflow.com/questions/72346829/two-problems-rendering-a-qmd-file-with-quarto-render-from-r


#' render html
#'
#' @param producerId
#'
#' @export
#'
#' @examples
#' unique(results22$producerId) |>
#' as.character() |>
#' walk(render_html)

render_html <- function(producerId) {
  box::use(here[here],
           glue[glue],
           quarto[quarto_render])
  quarto_render(
    input = paste0(
      here(),
      "/qmd/producer_report.qmd"
    ),
    output_format = "html",
    output_file = glue("qmd/{producerId}.html"),
    execute_params = list(
      producerId = producerId,
      year = params$year
    )
  )
}
