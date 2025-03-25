#' Theme for facetted strip plots
#'
#' @inheritParams make_texture_triangle
#' @param strip_color Color of facet strip background. Defaults to WaSHI blue.
#' @param strip_text_color Color of facet strip text. Defaults to white.
#' @param ... Other arguments to pass into [`ggplot2::theme()`].
#'
#' @export
#'
#' @examples
#' # Read in wrangled example plot data
#' df_plot_path <- soils_example("df_plot.RDS")
#' df_plot <- readRDS(df_plot_path)
#'
#' # Subset df to just biological measurement group
#' df_plot_bio <- df_plot |>
#'   dplyr::filter(measurement_group == "biological")
#'
#' # Make strip plot with all measurements and set scales based on
#' # the category column and then apply theme.
#'
#' # NOTE: the plot gets piped into the `set_scales()` function, which gets
#' # added to `theme_facet_strip()`.
#'
#' make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   fill = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales() +
#'   theme_facet_strip(body_font = "sans")
#'
#' # Example without setting theme
#' make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   fill = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales()
theme_facet_strip <- function(
  ...,
  body_font = "Poppins",
  strip_color = "#335c67",
  strip_text_color = "white"
) {
  theme <- ggplot2::theme(
    # Font family
    text = ggplot2::element_text(family = body_font),
    # Gridlines formatting
    panel.grid.major.x = ggplot2::element_blank(),
    # Axis formatting
    axis.title = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    # Facet label formatting
    strip.background = ggplot2::element_rect(
      fill = strip_color
    ),
    strip.text = ggtext::element_markdown(
      face = "bold",
      color = strip_text_color,
      size = 8,
      lineheight = 1.5
    ),
    # Legend formatting
    legend.title = ggplot2::element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.box.spacing = ggplot2::unit(0, "pt"),
    legend.spacing.x = ggplot2::unit(0, "pt"),
    # Any other arguments
    ...
  )
}

#' Define styles for producer's samples versus all samples
#'
#' @param plot `ggplot` object to apply scales to.
#' @param primary_color Color of producer's sample points. Defaults to WaSHI
#'   red
#' @param secondary_color Color of sample points with `"Same crop"` or `"Same
#'   county"` values in the `category` column. Defaults to WaSHI gray.
#' @param other_color Color of sample points with `"Other fields"` value in
#'   `category` column. Defaults to WaSHI tan.
#' @param language Language of the legend. `"English"` (default) or `"Spanish"`.
#'
#' @returns `ggplot` object with manual alpha, color, shape, and size scales
#'   applied.
#' @export
#'
#' @examples
#' # Read in wrangled example plot data
#' df_plot_path <- soils_example("df_plot.RDS")
#' df_plot <- readRDS(df_plot_path)

#' # Subset df to just biological measurement group
#' df_plot_bio <- df_plot |>
#'   dplyr::filter(measurement_group == "biological")
#'
#' # Make strip plot
#' make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   fill = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales() +
#'   theme_facet_strip(body_font = "sans")
#'
#' # Example without setting scales
#' make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label
#' ) +
#'   theme_facet_strip(body_font = "sans")
set_scales <- function(
  plot,
  primary_color = "#a60f2d",
  secondary_color = "#3E3D3D",
  other_color = "#ccc29c",
  language = "English"
) {
  # Language arg must be "English" or "Spanish"
  rlang::arg_match(
    arg = language,
    values = c("English", "Spanish")
  )

  if (language == "English") {
    plot <- plot +
      ggplot2::scale_alpha_manual(values = c(
        "Your fields" = 1,
        "Same county" = 0.6,
        "Same crop" = 0.6,
        "Other fields" = 0.5
      )) +
      ggplot2::scale_color_manual(values = c(
        "Your fields" = glue::glue(primary_color, "FF"),
        "Same county" = glue::glue("#494646", "CC"),
        "Same crop" = glue::glue("#918D8D", "CC"),
        "Other fields" = glue::glue(other_color, "CC")
      )) +
      ggplot2::scale_fill_manual(values = c(
        "Your fields" = glue::glue(primary_color, "FF"),
        "Same county" = glue::glue("#494646", 99),
        "Same crop" = glue::glue("#918D8D", 99),
        "Other fields" = glue::glue(other_color, 99)
      )) +
      ggplot2::scale_shape_manual(values = c(
        "Your fields" = 22,
        "Same county" = 8,
        "Same crop" = 24,
        "Other fields" = 21
      )) +
      ggplot2::scale_size_manual(values = c(
        "Your fields" = 3,
        "Same county" = 2.7,
        "Same crop" = 1.5,
        "Other fields" = 1.7
      ))
  }

  if (language == "Spanish") {
    plot <- plot +
      ggplot2::scale_alpha_manual(values = c(
        "Su campos" = 1,
        "Mismo contado" = 0.6,
        "Mismo cultivo" = 0.6,
        "Otros campos" = 0.5
      )) +
      ggplot2::scale_color_manual(values = c(
        "Su campos" = glue::glue(primary_color, "FF"),
        "Mismo contado" = glue::glue("#494646", "CC"),
        "Mismo cultivo" = glue::glue("#918D8D", "CC"),
        "Otros campos" = glue::glue(other_color, "CC")
      )) +
      ggplot2::scale_fill_manual(values = c(
        "Su campos" = glue::glue(primary_color, "FF"),
        "Mismo contado" = glue::glue("#494646", 99),
        "Mismo cultivo" = glue::glue("#918D8D", 99),
        "Otros campos" = glue::glue(other_color, 99)
      )) +
      ggplot2::scale_shape_manual(values = c(
        "Su campos" = 22,
        "Mismo contado" = 8,
        "Mismo cultivo" = 24,
        "Otros campos" = 21
      )) +
      ggplot2::scale_size_manual(values = c(
        "Su campos" = 3,
        "Mismo contado" = 2.7,
        "Mismo cultivo" = 1.5,
        "Otros campos" = 1.7
      ))
  }
  return(plot)
}

#' Make a facetted strip plot
#'
#' @param df Data frame to plot.
#' @param x Column for x-axis. For these strip plots, we recommend using a dummy
#'   variable to act as a placeholder. Defaults to a column named `dummy` with
#'   only one value ("dummy") for all rows.
#' @param y Column for y-axis. Defaults to `value`.
#' @param id Column with unique identifiers for each sample to use as `data_id`
#'   for interactive plots. Defaults to `sample_id`.
#' @param group Column to facet by. Defaults to `abbr_unit`.
#' @param tooltip Column with tooltip labels for interactive plots.
#' @param language Language of the legend. `"English"` (default) or `"Spanish"`.
#' @inheritParams add_texture_points
#' @inheritParams format_ft_colors
#' @returns Facetted `ggplot2` strip plots.
#' @export
#'
#' @examples
#' # Read in wrangled example plot data
#' df_plot_path <- soils_example("df_plot.RDS")
#' df_plot <- readRDS(df_plot_path)
#'
#' # Subset df to just biological measurement group
#' df_plot_bio <- df_plot |>
#'   dplyr::filter(measurement_group == "biological")
#'
#' # Make strip plot with all measurements and set scales based on
#' # the category column and then apply theme.
#'
#' # NOTE: the plot gets piped into the `set_scales()` function, which gets
#' # added to `theme_facet_strip()`.
#'
#' make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   fill = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales() +
#'   theme_facet_strip(body_font = "sans")
#'
#' # Example of strip plot without scales or theme functions
#' make_strip_plot(df_plot_bio)
#'
#' # Example of strip plot with `x` set to the facet group instead of a dummy
#' # variable. The dummy variable is what centers the points within the subplot.
#' make_strip_plot(
#'   df_plot_bio,
#'   x = abbr_unit,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   fill = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales() +
#'   theme_facet_strip(body_font = "sans")
make_strip_plot <- function(
  df,
  ...,
  x = dummy,
  y = value,
  id = sample_id,
  group = abbr_unit,
  tooltip = label,
  language = "English"
) {
  # Language arg must be "English" or "Spanish"
  rlang::arg_match(
    arg = language,
    values = c("English", "Spanish")
  )

  if (language == "English") {
    avg <- "Average"
    vals <- c("Average" = "dashed")
  }

  if (language == "Spanish") {
    avg <- "Promedio"
    vals <- c("Promedio" = "dashed")
  }

  # Set number of columns in facet
  n_facets <- df |>
    dplyr::select({{ group }}) |>
    dplyr::n_distinct()

  ncols <- ifelse(n_facets > 6, 4, 3)

  # Find project average for each measurement
  averages <- df |>
    dplyr::summarize(
      mean = mean({{ y }}, na.rm = TRUE),
      .by = c({{ group }}, unit)
    )

  df |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = {{ x }},
        y = {{ y }},
        ...
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars({{ group }}),
      scales = "free_y",
      ncol = ncols,
      labeller = ggplot2::label_wrap_gen()
    ) +
    ggiraph::geom_hline_interactive(
      data = averages,
      mapping = ggplot2::aes(
        yintercept = mean,
        linetype = avg,
        data_id = {{ group }},
        tooltip = glue::glue("{avg}: {round(mean, 2)} {unit}")
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = vals
    ) +
    ggiraph::geom_jitter_interactive(
      mapping = ggplot2::aes(
        data_id = {{ id }},
        tooltip = {{ tooltip }},
        hover_nearest = TRUE
      )
    ) +
    ggplot2::theme_bw()
}

#' Convert a `ggplot2` plot to an interactive `ggiraph`
#'
#' @param plot `ggplot2` plot to convert to interactive `ggiraph`. `plot` must
#'   contain `ggiraph::geom_<plot_type>_interactive()`.
#' @param body_font Font family to use throughout plot. Defaults to
#'   `"Poppins"`.
#' @param width Width of SVG output in inches. Defaults to 6.
#' @param height Height of SVG output in inches. Defaults to 4.
#' @param ... Other arguments passed to [`ggiraph::girafe_options()`].
#'
#' @returns Facetted strip plots with classes of `girafe` and `htmlwidget`.
#' @export
#'
#' @examples
#' # Read in wrangled example plot data
#' df_plot_path <- soils_example("df_plot.RDS")
#' df_plot <- readRDS(df_plot_path)
#'
#' # Make strip plot with all measurements and set scales based on
#' # the category column and then apply theme.
#'
#' # Subset df to just biological measurement group
#' df_plot_bio <- df_plot |>
#'   dplyr::filter(measurement_group == "biological")
#'
#' # NOTE: the plot gets piped into the `set_scales()` function, which gets
#' # added to `theme_facet_strip()`.

#' plot <- make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sample_id,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   fill = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales() +
#'   theme_facet_strip(body_font = "sans")
#'
#' # Convert static plot to interactive `ggiraph`
#' convert_ggiraph(plot)
convert_ggiraph <- function(
  plot,
  ...,
  body_font = "Poppins",
  width = 6,
  height = 4
) {
  if (!ggiraph::font_family_exists(body_font)) {
    cli::cli_inform(
      c(
        "Can't find font family {.arg {body_font}} on your system.",
        "i" = "Defaulting to a sans-serif font.",
        "i" = "See the \\
        {.href [ggiraph book](https://www.ardata.fr/ggiraph-book/fonts.html)} \\
        for help."
      )
    )
    body_font <- ggiraph::validated_fonts()$sans
  }

  tooltip_css <- glue::glue(
    "font-family:{body_font};font-size=1em;padding:5px;border-radius:6px;background-color:#1F1E1E;color:white;"
  )

  ggiraph::girafe(
    ggobj = plot,
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_tooltip(css = tooltip_css),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_hover(css = "fill-opacity:1;stroke:#1F1E1E"),
      ...
    )
  )
}
