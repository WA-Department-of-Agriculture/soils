# Copy R files to template folder
files <- c("helpers.R", "map.R", "plots.R", "render.R", "tables.R")
fs::file_copy(
  path = paste0(here::here(), "/R/", files),
  new_path = paste0(here::here(), "/inst/template/R/", files),
  overwrite = TRUE
)

# Check packages are installed manually with rlang since they are used only in
# Quarto

rlang::check_installed(
  "cli",
  reason = "for pretty messages to be printed in the console."
)

rlang::check_installed(
  "extrafont",
  reason = "to use Poppins and Lato fonts in reports."
)

rlang::check_installed(
  "purrr",
  reason = "to map functions necessary for data wrangling."
)

rlang::check_installed(
  "tidyr",
  reason = "to pivot data for wrangling."
)

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
