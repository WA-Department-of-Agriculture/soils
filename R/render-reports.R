#' Render producer reports in batch
#'
#' Renders HTML and/or Word versions of the producer report for every
#' unique year and producer combination in the dataset.
#'
#' @param data A data frame containing at least `year` and `producer_id`
#'   columns.
#' @param input_qmd Path to the Quarto report template.
#'   Defaults to `"01_producer-report.qmd"`.
#' @param output_dir Directory where rendered reports should be moved.
#'   Defaults to `"reports"`.
#' @param formats Character vector of output formats to render.
#'   Defaults to `c("html", "docx")`.
#' @param move_reports Logical. If `TRUE`, move rendered reports into
#'   `output_dir`. Defaults to `TRUE`.
#'
#' @return Invisibly returns a tibble containing report render metadata.
#'
#' @examples
#' \dontrun{
#' # Read processed data
#' data <- readRDS("data/data-processed.rds")
#' data <- input$results_wide
#' # Render all reports in both HTML and MS Word formats
#' render_reports(data, formats = c("html", "docx"))
#' }
#'
#' @export
render_reports <- function(
  data,
  input_qmd = "01_producer-report.qmd",
  output_dir = "reports",
  formats = c("html", "docx"),
  move_reports = TRUE
) {
  # Validate required columns
  required_cols <- c("year", "producer_id")

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Missing required columns in {.arg data}.",
      "!" = "Missing: {.val {missing_cols}}"
    ))
  }

  # Create report metadata dataframe
  reports <- data |>
    dplyr::distinct(year, producer_id) |>
    tidyr::crossing(output_format = formats) |>
    dplyr::mutate(
      output_file = glue::glue(
        "{year}_{producer_id}-report.{output_format}"
      ),
      execute_params = purrr::map2(
        year,
        producer_id,
        \(year, producer_id) {
          list(
            year = year,
            producer_id = producer_id
          )
        }
      )
    ) |>
    dplyr::select(
      output_file,
      output_format,
      execute_params
    )

  # Render reports
  reports |>
    purrr::pwalk(
      quarto::quarto_render,
      input = input_qmd,
      .progress = TRUE
    )

  # Move reports if requested
  if (isTRUE(move_reports)) {
    output_dir <- fs::dir_create(
      here::here(output_dir)
    )

    files <- fs::dir_ls(
      here::here(),
      regexp = paste0(
        "\\.(",
        paste(formats, collapse = "|"),
        ")$"
      )
    )

    fs::file_move(
      path = files,
      new_path = output_dir
    )

    cli::cli_alert_success(
      "Reports generated and moved to {.path {output_dir}}"
    )
  }

  invisible(reports)
}
