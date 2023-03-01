#' Pull unique values from one column of dataframe
#'
#' @param data dataframe with column to extract unique values from
#' @param target variable to pull unique vector of (crop or county)
#'
#' @export
#'

pull_unique <- function(data, target) {
  box::use(dplyr[pull])

  data |>
    pull({{ target }}) |>
    unique()
}
