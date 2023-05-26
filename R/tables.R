#' Conditionally flextable background cell colors.
#'
#' Color the background cells based on how the value compares
#' to the project average (as long as the project average is the last
#' row of the table).
#'
#' @param ft Flextable object
#' @param lighter_col Lighter background color. Defaults to WaSHI cream.
#' @param darker_col Darker background color. Defaults to WaSHI tan.
#'
#' @export
#'

format_ft_colors <- function(
    ft,
    lighter_col = washi::washi_pal[["standard"]][["cream"]],
    darker_col = washi::washi_pal[["standard"]][["tan"]]) {
  # Color formatter function
  ft <- flextable::bg(ft,
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
  ft <- flextable::compose(ft,
    i = 1, j = 1, part = "footer",
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
#' @param header_font Font of header text. Defaults to Lato.
#' @param body_font Font of body text. Defaults to Poppins.
#' @param header_color Background color of header cells. Defaults to
#'   WaSHI green.
#' @param border_color Color of border lines (programmatically
#'   darkened from selected color). Defaults to WaSHI tan.
#' @param ft Flextable object.
#'
#' @returns Styled flextable object.
#'
#' @export
#'

style_ft <- function(ft,
                     header_font = "Lato",
                     body_font = "Poppins",
                     header_color = washi::washi_pal[["standard"]][["green"]],
                     border_color = washi::washi_pal[["standard"]][["tan"]]) {
  flextable::set_flextable_defaults(font.family = body_font,
                                    font.size = 10)

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
        color = colorspace::darken(border_color,
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
    flextable::autofit(add_w = 0.05)

  return(ft)
}

#' Make flextable for specified 'measurement_group'.
#'
#' This function requires the functions in 'prepare_data.R' to be run
#' first.
#'
#' @param measurement_group Name of measurement group to visualize in
#'   flextable.
#'
#' @export
#'

make_ft <- function(measurement_group) {
  tables[[measurement_group]] |>
    flextable::flextable() |>
    flextable::set_header_df(
      mapping = headers[[measurement_group]], key = "key"
    ) |>
    soils::format_ft_colors() |>
    soils::style_ft()
}

#' Add bottom border to specific columns in flextable
#'
#' Use when columns with the same units are merged together.
#'
#' @param ft flextable object
#' @param columns Indices of columns that were merged.
#'
#' @returns Flextable object with bottom borders added.
#' @export
#'

unit_hline <- function(ft, columns) {
  flextable::hline(ft,
    i = 1, j = columns, part = "header",
    border = officer::fp_border(color = "white")
  )
}
