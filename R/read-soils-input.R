#' Read soils Excel input file
#'
#' Reads a single Excel file containing both the `Data` and
#' `Data Dictionary` sheets and performs readability validation checks.
#'
#' @param file Character path to a `.xlsx` file.
#' @param output Character. Output format for validation messages.
#'   One of `"cli"` (default) or `"ui"`.
#'
#' @returns
#' A named list with:
#' \describe{
#'   \item{data}{A data frame containing the `Data` sheet, or `NULL` if unreadable.}
#'   \item{data_dict}{A data frame containing the `Data Dictionary` sheet, or `NULL` if unreadable.}
#'   \item{issues}{A list of validation issue objects. Empty if no issues were found.}
#' }
#'
#' @keywords internal
read_soils_excel <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)

  issues <- list()
  data <- NULL
  data_dict <- NULL

  wb <- tryCatch(
    openxlsx2::wb_load(file),
    error = function(e) NULL
  )

  sheets <- tryCatch(
    openxlsx2::wb_get_sheet_names(wb),
    error = function(e) NULL
  )

  if (is.null(wb) || is.null(sheets)) {
    issues <- c(
      issues,
      list(new_issue(
        "error",
        "Could not read Excel file."
      ))
    )

    return(list(
      data = NULL,
      data_dict = NULL,
      issues = issues
    ))
  }

  required <- c("Data", "Data Dictionary")
  missing <- setdiff(required, sheets)

  if (length(missing) > 0) {
    msg <- cli::format_inline(
      "Missing required sheets: {.val {missing}}"
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )

    return(list(
      data = NULL,
      data_dict = NULL,
      issues = issues
    ))
  }

  data <- tryCatch(
    openxlsx2::wb_to_df(wb, "Data"),
    error = function(e) NULL
  )

  data_dict <- tryCatch(
    openxlsx2::wb_to_df(wb, "Data Dictionary"),
    error = function(e) NULL
  )

  if (is.null(data) && is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read both Data and Data Dictionary sheets."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  } else if (is.null(data)) {
    msg <- cli::format_inline(
      "Could not read Data sheet."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  } else if (is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read Data Dictionary sheet."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  }

  list(
    data = data,
    data_dict = data_dict,
    issues = issues
  )
}


#' Read CSV input files
#'
#' Reads two CSV files: one containing the data and one containing the
#' data dictionary. Performs file name and readability validation checks.
#'
#' @param file Character vector of length 2.
#' @param output Character. Output format for validation messages.
#'   One of `"cli"` (default) or `"ui"`.
#'
#' @returns
#' A named list with:
#' \describe{
#'   \item{data}{A data frame containing the data file, or `NULL` if unreadable.}
#'   \item{data_dict}{A data frame containing the data dictionary file, or `NULL` if unreadable.}
#'   \item{issues}{A list of validation issue objects. Empty if no issues were found.}
#' }
#'
#' @keywords internal
read_soils_csv <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)

  issues <- list()
  data <- NULL
  data_dict <- NULL

  if (!grepl("data", file[1], ignore.case = TRUE)) {
    msg <- cli::format_inline(
      "First file must be the data file and include {.val data} in the filename. You provided: {.val {file[1]}}."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  }

  if (!grepl("dictionary", file[2], ignore.case = TRUE)) {
    msg <- cli::format_inline(
      "Second file must be the data dictionary and include {.val dictionary} in the filename. You provided: {.val {file[2]}}."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  }

  if (length(issues) > 0) {
    return(list(
      data = NULL,
      data_dict = NULL,
      issues = issues
    ))
  }

  data <- tryCatch(
    utils::read.csv(
      file[1],
      check.names = FALSE,
      encoding = "UTF-8",
      strip.white = TRUE
    ),
    error = function(e) NULL
  )

  data_dict <- tryCatch(
    utils::read.csv(
      file[2],
      check.names = FALSE,
      encoding = "UTF-8",
      strip.white = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(data) && is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read both {.file {basename(file[1])}} and {.file {basename(file[2])}}."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  } else if (is.null(data)) {
    msg <- cli::format_inline(
      "Could not read {.file {basename(file[1])}}."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  } else if (is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read {.file {basename(file[2])}}."
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  }

  list(
    data = data,
    data_dict = data_dict,
    issues = issues
  )
}

#' Read soils input data
#'
#' Detects input type and reads soils data from either:
#' \itemize{
#'   \item A single Excel file (`.xlsx`) containing `Data` and
#'   `Data Dictionary` sheets, or
#'   \item Two CSV files: one data file and one data dictionary file
#' }
#'
#' @param file Character vector. Either:
#'   \itemize{
#'     \item Length 1: path to a `.xlsx` file
#'     \item Length 2: paths to two `.csv` files (data first, dictionary second)
#'   }
#' @param output Character. Output format for validation messages.
#'   One of `"cli"` (default) or `"ui"`.
#'
#' @returns
#' A named list with:
#' \describe{
#'   \item{data}{Data frame of input data, or `NULL` if unreadable.}
#'   \item{data_dict}{Data frame of data dictionary, or `NULL` if unreadable.}
#'   \item{issues}{A formatted list of validation issue objects. Empty if no issues were found.}
#'   \item{source}{Character string indicating detected input type: `"excel"` or `"csv"`.}
#'   \item{file}{Original input file path(s).}
#'   \item{passed}{Logical indicating whether the input passed all initial loading and readability checks.}
#' }
#'
#' @details
#' This function performs only input loading and readability validation.
#' Structural validation is performed separately by
#' `check_input_structure()`.
#'
#' @export
read_soils_input <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)

  issues <- list()

  is_excel <- (length(file) == 1 &&
    grepl("\\.xlsx$", file, ignore.case = TRUE))

  is_csv <- (length(file) == 2 &&
    all(grepl("\\.csv$", file, ignore.case = TRUE)))

  if (!is_excel && !is_csv) {
    msg <- c(
      "Provide either:",
      cli::format_inline(
        "One {.strong .xlsx} file with {.val Data} and {.val Data Dictionary} sheets, or"
      ),
      cli::format_inline(
        "Two {.strong .csv} files with data first and dictionary second as {.code c(\"data.csv\", \"data-dictionary.csv\")}."
      )
    )

    issues <- c(
      issues,
      list(new_issue("error", msg))
    )
  }

  if (isTRUE(is_excel)) {
    input <- read_soils_excel(file, output = output)
  }

  if (isTRUE(is_csv)) {
    input <- read_soils_csv(file, output = output)
  }

  return(list(
    data = input$data,
    data_dict = input$data_dict,
    issues = issues,
    source = if (is_excel) "excel" else "csv",
    file = file,
    passed = length(issues) == 0
  ))
}
