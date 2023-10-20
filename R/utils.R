#' Get path to example data
#'
#' `soils` comes bundled with some example files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @source Adapted from `readxl::readxl_example()`.
#' @examples
#' soils_example()
#'
#' soils_example("dfPlot.csv")
soils_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(soils_sys("template/extdata"))
  } else {
    soils_sys(paste0("template/extdata/", file))
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

cat_info <- function(...) {
  cli::cat_bullet(
    ...,
    bullet = "arrow_right",
    bullet_col = "grey"
  )
}


cat_exists <- function(where) {
  cat_red_bullet(
    sprintf(
      "[Skipped] %s already exists.",
      basename(where)
    )
  )
  cat_info(
    sprintf(
      "If you want replace it, remove the %s file first.",
      basename(where)
    )
  )
}

cat_dir_necessary <- function() {
  cat_red_bullet(
    "File not added (needs a valid directory)"
  )
}

cat_start_download <- function() {
  cli::cat_line("")
  cli::cat_line("Initiating file download")
}

cat_downloaded <- function(
  where,
  file = "File"
    ) {
  cat_green_tick(
    sprintf(
      "%s downloaded at %s",
      file,
      where
    )
  )
}

cat_start_copy <- function() {
  cli::cat_line("")
  cli::cat_line("Copying file")
}

cat_copied <- function(
  where,
  file = "File"
    ) {
  cat_green_tick(
    sprintf(
      "%s copied to %s",
      file,
      where
    )
  )
}

cat_created <- function(
  where,
  file = "File"
    ) {
  cat_green_tick(
    sprintf(
      "%s created at %s",
      file,
      where
    )
  )
}
