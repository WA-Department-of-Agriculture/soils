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
#' If successful, a named list with:
#' \describe{
#'   \item{data}{A data frame containing the `Data` sheet.}
#'   \item{data_dict}{A data frame containing the `Data Dictionary` sheet.}
#' }
#'
#' If validation fails, returns a formatted issue object via
#' `format_output()`.
#'
#' @keywords internal
read_soils_excel <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  wb <- tryCatch(
    openxlsx2::wb_load(file),
    error = function(e) NULL
  )

  sheets <- tryCatch(
    openxlsx2::wb_get_sheet_names(wb),
    error = function(e) NULL
  )

  if (is.null(wb) || is.null(sheets)) {
    return(format_output(
      list(new_issue("error", "Could not read Excel file.")),
      output,
      context = list(
        error = "Failed to load input data.",
        warning = ""
      )
    ))
  }

  required <- c("Data", "Data Dictionary")
  missing <- setdiff(required, sheets)

  if (length(missing) > 0) {
    msg <- cli::format_inline(
      "Missing required sheets: {.val {missing}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(format_output(
      issues,
      output,
      context = list(
        error = "Failed to load input data.",
        warning = ""
      )
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

    issues <- c(issues, list(new_issue("error", msg)))
  } else if (is.null(data)) {
    msg <- cli::format_inline(
      "Could not read Data sheet"
    )

    issues <- c(issues, list(new_issue("error", msg)))
  } else if (is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read Data Dictionary sheet."
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (length(issues) > 0) {
    return(format_output(
      issues,
      output,
      context = list(
        error = "Failed to load input data."
      )
    ))
  }

  return(list(data = data, data_dict = data_dict))
}


#' Read CSV input files
#'
#' Reads two CSV files: one containing the data and one containing the
#' data dictionary. Performs file name and readability validation checks.
#'
#' @param file Character vector of length 2. Paths to CSV files, where:
#'   \itemize{
#'     \item The first file must contain `"data"` in the filename.
#'     \item The second file must contain `"dictionary"` in the filename.
#'   }
#' @param output Character. Output format for validation messages.
#'   One of `"cli"` (default) or `"ui"`.
#'
#' @returns
#' If successful, a named list with:
#' \describe{
#'   \item{data}{A data frame created from the first CSV file.}
#'   \item{data_dict}{A data frame created from the second CSV file.}
#' }
#'
#' If validation fails, returns a formatted issue object via
#' `format_output()`.
#'
#' @keywords internal
read_soils_csv <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  if (!grepl("data", file[1], ignore.case = TRUE)) {
    msg <- cli::format_inline(
      "First file must be the data file and include {.val data} in the filename. You provided: {.val {file[1]}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (!grepl("dictionary", file[2], ignore.case = TRUE)) {
    msg <- cli::format_inline(
      "Second file must be the data dictionary and include {.val dictionary} in the filename. You provided: {.val {file[2]}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (length(issues) > 0) {
    return(format_output(
      issues,
      output,
      context = list(
        error = "Failed to load input data.",
        warning = ""
      )
    ))
  }

  data <- tryCatch(
    read.csv(
      file[1],
      check.names = FALSE,
      encoding = "UTF-8",
      strip.white = TRUE
    ),
    error = function(e) NULL
  )

  data_dict <- tryCatch(
    read.csv(
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

    issues <- c(issues, list(new_issue("error", msg)))
  } else if (is.null(data)) {
    msg <- cli::format_inline(
      "Could not read {.file {basename(file[1])}}."
    )

    issues <- c(issues, list(new_issue("error", msg)))
  } else if (is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read {.file {basename(file[2])}}."
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (length(issues) > 0) {
    return(format_output(
      issues,
      output,
      context = list(
        error = "Failed to load input data.",
        warning = ""
      )
    ))
  }

  return(list(data = data, data_dict = data_dict))
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
#'     \item Length 1: path to a `.xlsx` file, or
#'     \item Length 2: paths to two `.csv` files (data first, dictionary second)
#'   }
#' @param output Character. Output format for validation messages.
#'   One of `"cli"` (default) or `"ui"`.
#'
#' @returns
#' If successful, a named list with:
#' \describe{
#'   \item{data}{Dataframe of input data.}
#'   \item{data_dict}{Dataframe of data dictionary.}
#'   \item{source}{Character string indicating input type: `"excel"` or `"csv"`.}
#'   \item{file}{Original input file path(s).}
#' }
#'
#' If validation fails, returns a formatted issue object via
#' `format_output()`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Excel input
#' read_soils_input("soil_data.xlsx")
#'
#' # CSV input
#' read_soils_input(c("data.csv", "data_dictionary.csv"))
#' }
read_soils_input <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  # Detect file type

  is_excel <- length(file) == 1 && grepl("\\.xlsx$", file, ignore.case = TRUE)
  is_csv <- length(file) == 2 && all(grepl("\\.csv$", file, ignore.case = TRUE))

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
    issues <- c(issues, list(new_issue("error", msg)))

    return(format_output(
      issues,
      output,
      context = list(
        error = "Invalid input.",
        warning = ""
      )
    ))
  }

  if (isTRUE(is_excel)) {
    input <- read_soils_excel(file, output = output)
  }

  if (isTRUE(is_csv)) {
    input <- read_soils_csv(file, output = output)
  }

  # Success

  return(list(
    data = input$data,
    data_dict = input$data_dict,
    source = if (is_excel) "excel" else "csv",
    file = file
  ))
}
