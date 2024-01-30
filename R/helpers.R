#' Calculate the mode of categorical variable
#'
#' @param x Character vector to calculate mode from.
#'
#' @returns The value that occurred most often.
#'
#' @examples
#' calculate_mode(washi_data$crop)
#'
#' @export
#'

calculate_mode <- function(x) {
  uniqx <- unique(stats::na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#' Pull unique values from one column of dataframe
#'
#' @param df Dataframe with column to extract unique values from.
#' @param target Variable to pull unique vector of (i.e. crop or
#'   county).
#'
#' @returns Vector of unique values from target column.
#' @examples
#' washi_data |>
#'   pull_unique(crop)
#'
#' @export
#'

pull_unique <- function(df, target) {
  df |>
    dplyr::pull({{ target }}) |>
    unique()
}

# These functions are used to wrangle data for the table and plot functions in
# producer_report.qmd ---------------------------------------------------------

#' Calculate n samples and most frequent texture by a grouping variable
#'
#' This function is used in `summarize_by_var`.
#'
#' @param results_long Dataframe in tidy, long format with columns: `sample_id`,
#'   `texture`.
#' @param producer_info Vector of producer's values for the grouping variable.
#' @param var Variable to group and summarize by.
#'
get_n_texture_by_var <- function(results_long, producer_info, var) {
  testthat::expect_contains(names(results_long), c("sample_id", "texture"))

  results_long |>
    dplyr::filter({{ var }} %in% producer_info) |>
    dplyr::summarize(
      n = dplyr::n_distinct(sample_id),
      Texture = calculate_mode(texture),
      .by = {{ var }}
    )
}

#' Summarize producer's samples with averages by grouping variable
#'
#' @param results_long Dataframe in tidy, long format with columns: `sample_id`,
#'   `texture`, `measurement_group`, `abbr`, `value`.
#' @param producer_samples Dataframe in tidy, long format with columns:
#'   `measurement_group`, `abbr`, `value`.
#' @param var Variable to summarize by.
#'
#' @export
summarize_by_var <- function(results_long, producer_samples, var) {
  testthat::expect_contains(
    names(producer_samples),
    c("measurement_group", "abbr", "value")
  )

  testthat::expect_contains(
    names(results_long),
    c("sample_id", "texture", "measurement_group", "abbr", "value")
  )

  producer_info <- producer_samples |>
    pull_unique({{ var }})

  n_texture <- get_n_texture_by_var(results_long, producer_info, {{ var }})

  results_long |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(measurement_group, abbr, {{ var }})
    ) |>
    dplyr::filter({{ var }} %in% producer_info) |>
    dplyr::left_join(n_texture, by = dplyr::join_by({{ var }})) |>
    dplyr::mutate(
      "Field or Average" = glue::glue("Average \n({n} Fields)")
    ) |>
    dplyr::select(-n) |>
    tidyr::unite(
      "Field or Average",
      c({{ var }}, `Field or Average`),
      sep = " "
    )
}

#' Summarize samples across the project
#'
#' @param results_long Dataframe in tidy, long format with columns: `sample_id`,
#'   `texture`, `measurement_group`, `abbr`, `value`.
#'
#' @export
summarize_by_project <- function(results_long) {
  testthat::expect_contains(
    names(results_long),
    c("sample_id", "texture", "measurement_group", "abbr", "value")
  )

  n <- dplyr::n_distinct(results_long$sample_id)

  texture <- calculate_mode(results_long$texture)

  results_long |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(measurement_group, abbr)
    ) |>
    dplyr::mutate(
      "Field or Average" = glue::glue("Project Average \n({n} Fields)"),
      Texture = texture
    )
}

#' Get table headers for flextable
#'
#' This function uses the data dictionary to create a new dataframe of the
#' abbreviations and units for each measurement group for flextable
#'
#' @param dictionary Dataframe containing columns `measurement_group`, `abbr`,
#'   `unit`.
#' @param group Character `measurement_group` value.
#'
#' @export
get_table_headers <- function(dictionary, group) {
  testthat::expect_contains(
    names(dictionary),
    c("measurement_group", "abbr", "unit")
  )

  dictionary |>
    dplyr::filter(measurement_group == group) |>
    dplyr::select(abbr, unit) |>
    dplyr::mutate(key = abbr, .after = abbr) |>
    rbind(c("Field or Average", "Field or Average", ""))
}
