#' Conditional formatting of flextable background cell colors
#'
#' Color the background cells based on how the value compares to the project
#' average. The project average must be the last row of the table. A footnote is
#' added to the table describing what the dark and light colors mean.
#'
#' @param ft Flextable object
#' @param lighter_col Lighter background color. Defaults to WaSHI cream.
#' @param darker_col Darker background color. Defaults to WaSHI tan.
#'
#' @export
#'
#' @examples
#' # Read in wrangled example table data.
#' # See `data_wrangling.R` for processing steps.
#' tables_path <- soils_example("tables.RDS")
#' tables <- readRDS(tables_path)
#'
#' # Make the table
#' ft <- flextable::flextable(tables$biological)
#' ft
#'
#' # Conditionally format the background cells
#' format_ft_colors(ft)
format_ft_colors <- function(
  ft,
  lighter_col = washi::washi_pal[["standard"]][["cream"]],
  darker_col = washi::washi_pal[["standard"]][["tan"]]
    ) {
  # Color formatter function
  ft <- flextable::bg(
    ft,
    bg = function(x) {
      dplyr::case_when(
        is.character(x) ~ "white",
        x < tail(x, n = 1) ~ lighter_col,
        x >= tail(x, n = 1) ~ darker_col,
        TRUE ~ "white"
      )
    }
  )

  # Add an empty footer line
  ft <- flextable::add_footer_lines(ft, values = "")

  # Add the footnote content, with the backgrounds highlighted
  ft <- flextable::compose(
    ft,
    i = 1,
    j = 1,
    part = "footer",
    value = flextable::as_paragraph(
      "Values \U2265 project average have ",
      flextable::as_highlight(
        "darker backgrounds. \n",
        darker_col
      ),
      "Values < project average have ",
      flextable::as_highlight(
        "lighter backgrounds. ",
        lighter_col
      )
    )
  )
  return(ft)
}

#' Style flextable
#'
#' @param header_font Font of header text. Defaults to `"Lato"`.
#' @param body_font Font of body text. Defaults to `"Poppins"`.
#' @param header_color Background color of header cells. Defaults to WaSHI
#'   green.
#' @param border_color Color of border lines. Defaults to WaSHI tan. Uses
#'   `{colorspace}` to programatically darken the given color since the default
#'   border color is the same as the darker background color in
#'   `format_ft_colors()`.
#' @param ft Flextable object.
#'
#' @returns Styled flextable object.
#'
#' @export
#'
#' @examples
#' # Read in wrangled example table data.
#' # See `data_wrangling.R` for processing steps.
#' tables_path <- soils_example("tables.RDS")
#' tables <- readRDS(tables_path)
#'
#' # Make the table
#' ft <- flextable::flextable(tables$biological)
#' ft
#'
#' # Style the table
#' style_ft(ft)
style_ft <- function(
  ft,
  header_font = "Lato",
  body_font = "Poppins",
  header_color = washi::washi_pal[["standard"]][["green"]],
  border_color = washi::washi_pal[["standard"]][["tan"]]
    ) {
  flextable::set_flextable_defaults(
    font.family = body_font,
    font.size = 10
  )

  header_cell <- officer::fp_cell(
    background.color = header_color
  )
  header_text <- officer::fp_text(
    font.family = header_font,
    font.size = 11,
    bold = TRUE,
    color = "white"
  )

  ft <- flextable::style(
    ft,
    pr_t = header_text,
    pr_c = header_cell,
    part = "header"
  ) |>
    flextable::bold(j = 1, bold = TRUE, part = "body") |>
    flextable::hline(
      border = officer::fp_border(
        color = colorspace::darken(
          border_color,
          amount = 0.3,
          space = "HCL",
        )
      ),
      part = "body"
    ) |>
    flextable::merge_h(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::line_spacing(space = 1.3, part = "all") |>
    flextable::width(j = 1, width = 0.75) |>
    flextable::autofit()

  return(ft)
}

#' Make a flextable with column names from another dataframe
#'
#' @param table A dataframe with the contents of the desired flextable output.
#' @param header Another dataframe with three columns:
#'  * First column contains what the top header row. In our template, this is the abbreviation of the measurement (i.e. `Organic Matter`).
#'  * Second column, called `"key"`, contains the join key. In our template, this is the same as the first column.
#' * Third column contains the second header row. In our template, this is the unit (i.e. `%`).
#'
#' @export
#' @returns Formatted flextable object.
#'
#' @examples
#' # Read in wrangled table data.
#' # See `data_wrangling.R` for processing steps.
#' headers_path <- soils_example("headers.RDS")
#' headers <- readRDS(headers_path)
#'
#' tables_path <- soils_example("tables.RDS")
#' tables <- readRDS(tables_path)
#'
#' # The data structure necessary to render the flextable
#' headers$chemical
#'
#' tables$chemical
#'
#' # Make the table
#' make_ft(table = tables$chemical, header = headers$chemical)
#'
#' # Note the line under the merged headers has not been added in this example.
#' # See the example for `unit_hline()`.
make_ft <- function(table, header) {
  table |>
    flextable::flextable() |>
    flextable::set_header_df(
      mapping = header,
      key = "key"
    ) |>
    format_ft_colors() |>
    style_ft()
}

#' Add bottom border to specific columns in flextable
#'
#' Use when columns with the same units are merged together to add a bottom
#' border to make it more obvious those columns share units.
#'
#' @param ft flextable object
#' @param columns Indices of columns that were merged.
#'
#' @returns Flextable object with bottom borders added.
#' @export
#'
#' @examples
#' # Read in wrangled table data.
#' # See `data_wrangling.R` for processing steps.
#' headers_path <- soils_example("headers.RDS")
#' headers <- readRDS(headers_path)
#'
#' tables_path <- soils_example("tables.RDS")
#' tables <- readRDS(tables_path)
#'
#' # Make the table
#' ft <- make_ft(table = tables$chemical, header = headers$chemical)
#' ft
#'
#' # Add a line under the merged columns with the same units
#' unit_hline(ft, 5)
unit_hline <- function(ft, columns) {
  flextable::hline(
    ft,
    i = 1,
    j = columns,
    part = "header",
    border = officer::fp_border(color = "white")
  )
}
