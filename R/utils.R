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
      "extdata", file,
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
