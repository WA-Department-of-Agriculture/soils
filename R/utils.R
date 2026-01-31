#' Get path to example data
#'
#' `soils` comes bundled with some example files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @source Adapted from `readxl::readxl_example()`.
#'
#' @export
#'
#' @examples
#' soils_example()
#'
#' soils_example("df_plot.RDS")
soils_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "soils"))
  } else {
    system.file(
      "extdata",
      file,
      package = "soils",
      mustWork = TRUE
    )
  }
}

# Below functions from
# https://github.com/ThinkR-open/golem/blob/365a5cc303b189973abab0dd375c64be79bcf74a/R/utils.R

soils_sys <- function(
  ...,
  lib.loc = NULL,
  mustWork = FALSE
) {
  system.file(
    ...,
    package = "soils",
    lib.loc = lib.loc,
    mustWork = mustWork
  )
}

cat_green_tick <- function(...) {
  cli::cat_bullet(
    ...,
    bullet = "tick",
    bullet_col = "green"
  )
}

cat_red_bullet <- function(...) {
  cli::cat_bullet(
    ...,
    bullet = "bullet",
    bullet_col = "red"
  )
}

soils_cli_vec <- function(x) {
  cli::cli_vec(x, list(vec_trunc = 5))
}

abort_if_missing_cols <- function(
  df,
  cols,
  context = NULL,
  arg = rlang::caller_arg(df),
  call = rlang::caller_env()
) {
  missing <- setdiff(cols, names(df))

  if (length(missing) == 0) {
    return(invisible(df))
  }

  if (length(missing) == 1) {
    msg <- c(
      "x" = "{.arg {arg}} must have the required column: {.field {missing}}"
    )
  }

  if (length(missing) == 2) {
    msg <- c(
      "x" = "{.arg {arg}} must have the required columns: {.field {missing[1]}} and\
      {.field {missing[2]}}"
    )
  }

  if (length(missing) >= 3) {
    msg <- c(
      "x" = "{.arg {arg}} must have the required columns:",
      rlang::set_names(
        paste0("{.field ", missing, "}"),
        rep("*", length(missing))
      )
    )
  }

  if (!is.null(context)) {
    msg <- c(msg, "i" = context)
  }

  cli::cli_abort(msg, call = rlang::caller_env())
}
