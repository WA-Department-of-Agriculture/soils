#' Calculate mode of categorical variable
#'
#' @param x Character vector to calculate mode from.
#'
#' @returns The value that occurred most often.
#'
#' @examples
#' calculate_mode(example_data_wide$crop)
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
#' example_data_wide |>
#'   pull_unique(crop)
#'
#' @export
#'

pull_unique <- function(df, target) {
  df |>
    dplyr::pull({{ target }}) |>
    unique()
}
