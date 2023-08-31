#' Create a project directory for soil health reports using a Quarto template
#'
#' This function was adapted from
#' https://github.com/ThinkR-open/golem/blob/365a5cc303b189973abab0dd375c64be79bcf74a/R/create_golem.R
#'
#' @param path Name of project directory to be created.
#' @param overwrite Boolean. Overwrite the existing project?
#' @param open Boolean. Open the newly created project?
#'
#' @return A new project directory containing template and resources.
#' @export
#'
#' @examples
#' \dontrun{
#' create_soils_project(tempdir("soils"))
#' }
create_soils <- function(
  path,
  overwrite = FALSE,
  open = TRUE
    ) {
  path <- normalizePath(
    path,
    mustWork = FALSE
  )

  if (fs::dir_exists(path)) {
    if (!isTRUE(overwrite)) {
      stop(
        paste(
          "Project directory already exists.\n",
          "Set `create_soils_project(overwrite = TRUE)` to overwrite anyway."
        ),
        call. = FALSE
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

  template <- soils_sys("template")

  # Copy over whole directory
  fs::dir_copy(
    path = template,
    new_path = paste0(path),
    overwrite = TRUE
  )

  # Delete random files from package build and sysdata
  files_delete <- c(
    "soils",
    "soils.rdb",
    "soils.rdx",
    "sysdata.rdb",
    "sysdata.rdx"
  )

  invisible(purrr::map(
    files_delete,
    \(file) {
      path <- paste0(path, "/R/", file)
      if (fs::file_exists(path)) {
        fs::file_delete()
      }
    }
  ))
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
