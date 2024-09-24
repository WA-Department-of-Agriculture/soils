#' Create a project directory for generating soil health reports
#'
#' Creates an RStudio project containing Quarto template and resources (images,
#' style sheets, render.R script).
#'
#' @param path Name of project directory to be created.
#' @param template Template type. Either "English" or "Spanish".
#' @param overwrite Boolean. Overwrite the existing project?
#' @param open Boolean. Open the newly created project?
#'
#' @source Adapted from `golem::create_golem()`.
#'
#' @return A new project directory containing template and resources.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create temporary directory
#' dir <- tempdir()
#'
#' # Create soils project
#' create_soils(dir, overwrite = TRUE)
#'
#' # Delete temporary directory
#' unlink(dir, recursive = TRUE)
#' }
create_soils <- function(
  path,
  template = "English",
  overwrite = FALSE,
  open = TRUE
    ) {
  if (missing(path)) {
    cli::cli_abort(c(
      "!" = "{.arg path} must be provided.",
      "i" = "Where do you want to create this project?",
      "i" = "For example, {.code create_soils(path = 'path/to/my/directory')}."
    ))
  }

  # template arg must be "english" or "spanish"
  rlang::arg_match(
    arg = template,
    values = c("English", "Spanish")
  )

  path <- fs::path_norm(path)

  dir_exists <- fs::dir_exists(path)
  overwriting <- isTRUE(overwrite)

  if (dir_exists && !overwriting) {
    cli::cli_abort(
      c(
        "!" = "{.path {path}} already exists.",
        "i" = "To overwrite, use: \\
          {.code create_soils({.str {path}}, overwrite = TRUE)}"
      )
    )
  }

  if (dir_exists && overwriting) {
    cat_red_bullet("Overwriting existing project.")
    if (rstudioapi::isAvailable() & isTRUE(open)) {
      rstudioapi::openProject(path, newSession = TRUE)
    }
  }

  if (!dir_exists) {
    cli::cat_rule("Creating directory")
    usethis::create_project(
      path = path,
      open = open
    )
  }

  cat_green_tick("Created project directory")

  invisible({
    template <- soils_sys(paste0("templates/", tolower(template)))

    # Copy over whole directory
    fs::dir_copy(
      path = template,
      new_path = path,
      overwrite = TRUE
    )
  })
}

#' Create a project directory for soil health reports using a Quarto template
#'
#' For use only in RStudio "New Project" GUI. Defaults to not overwrite and not
#' open the project.
#' @inheritParams create_soils
#' @noRd
create_soils_gui <- function(path, ...) {
  dots <- list(...)

  create_soils(
    path = path,
    template = dots$template,
    overwrite = FALSE,
    open = FALSE
  )
}
