#' Create a project directory for generating soil health reports
#'
#' Creates an RStudio project containing Quarto template and resources (images,
#' style sheets, render.R script).
#'
#' @param path Name of project directory to be created.
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
  overwrite = FALSE,
  open = TRUE
    ) {
  if (missing(path)) {
    cli::cli_abort(c(
      "!" = "`path` is missing.",
      "i" = "Where do you want to create this project?"
    ))
  }

  path <- normalizePath(
    path,
    mustWork = FALSE
  )

  if (fs::dir_exists(path)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort(
        c(
          "!" = "{path} already exists.",
          "i" = "Set `create_soils(overwrite = TRUE)` to overwrite anyway."
        )
      )
    } else {
      cat_red_bullet("Overwriting existing project.")
      if (rstudioapi::isAvailable() & isTRUE(open)) {
        rstudioapi::openProject(path, newSession = TRUE)
      }
    }
  } else {
    cli::cat_rule("Creating directory")
    usethis::create_project(
      path = path,
      open = open
    )
  }
  cat_green_tick("Created project directory")

  invisible({
    template <- soils_sys("template")

    # Copy over whole directory
    fs::dir_copy(
      path = template,
      new_path = paste0(path),
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
  create_soils(
    path = path,
    overwrite = FALSE,
    open = FALSE
  )
}
