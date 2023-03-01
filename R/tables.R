#' color_formatter#' color_formatter
#'
#' @param ft flextable object
#'
#' @export
#'

color_formatter <- function(ft) {
  box::use(
    flextable[bg, add_footer_lines, compose, as_paragraph, as_highlight],
    dplyr[mutate, case_when]
  )

  # this function colors the background based on how the value compares
  # to the project average (as long as the project average is the last
  # row of the table)

  ft <- bg(ft,
    bg = function(x) {
      case_when(
        is.character(x) ~ "transparent",
        x < tail(x, n = 1) ~ washi_pal$cream,
        x >= tail(x, n = 1) ~ washi_pal$tan,
        TRUE ~ "transparent"
      )
    }
  )

  # add an empty footer line

  ft <- add_footer_lines(ft, values = "")

  # add the footnote content, with the backgrounds highlighted

  ft <- compose(ft,
    i = 1, j = 1, part = "footer",
    value = as_paragraph(
      "Values ≥ project average have ",
      as_highlight(
        "darker backgrounds. \n",
        washi_pal$tan
      ),
      "Values < project average have ",
      as_highlight(
        "lighter backgrounds. ",
        washi_pal$cream
      )
    )
  )
  return(ft)
}

#' style flextable
#'
#' @param ft flextable object
#' @export
#'

style_ft <- function(ft,
                     header_color = washi_pal$green,
                     border_color = washi_pal$tan) {
  box::use(
    flextable[
      set_flextable_defaults, style, bold, hline, merge_h, align,
      width, line_spacing, autofit
    ],
    officer[fp_cell, fp_border, fp_text]
  )
  set_flextable_defaults(font.family = "Poppins")

  header_cell <- fp_cell(
    background.color = header_color
  )
  header_text <- fp_text(
    font.family = "Lato",
    font.size = 12,
    bold = TRUE,
    color = "white"
  )

  ft <- style(
    ft,
    pr_t = header_text,
    pr_c = header_cell,
    part = "header"
  ) |>
    bold(j = 1, bold = TRUE, part = "body") |>
    hline(border = fp_border(color = border_color), part = "body") |>
    merge_h(part = "header") |>
    align(align = "center", part = "header") |>
    line_spacing(space = 1.3, part = "all") |>
    width(j = 1, width = 1.2) |>
    autofit(add_w = 0.1)

  return(ft)
}

#' make flextable
#'
#' @param measurement_group
#'
#' @export
#'

make_ft <- function(measurement_group) {
  box::use(flextable[flextable, set_header_df])

  tables[[measurement_group]] |>
    flextable() |>
    set_header_df(mapping = headers[[measurement_group]], key = "key") |>
    color_formatter() |>
    style_ft()
}

#' unit_hline
#'
#' @param ft flextable object
#'
#' @param columns indices of columns that were merged to add border under header
#' @return flextable object
#' @export
#'

unit_hline <- function(ft, columns) {
  box::use(flextable[hline], officer[fp_border])
  hline(ft,
    i = 1, j = columns, part = "header",
    border = fp_border(color = "white")
  )
}
