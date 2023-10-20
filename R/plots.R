#' Make a texture triangle
#' @param body_font Font family to use throughout plot. Defaults to
#'   `"Poppins"`.
#'
#' @returns Blank `ggplot2` texture triangle with USDA textural classes.
#'
#' @export
#'
#' @examples
#' # Blank texture triangle with just USDA classes
#' make_texture_triangle(body_font = "sans")
#'
make_texture_triangle <- function(
    body_font = "Poppins") {
  if (!exists("usdaTexture")) {
    cli::cli_abort(c(
      "Can't find `usdaTexture`.",
      "i" = "Run library(soils)."
    ))
  }

  suppressWarnings({
    ggplot2::ggplot(
      data = usdaTexture$polygons,
      mapping = ggplot2::aes(
        x = sand,
        y = clay,
        z = silt
      )
    ) +
      ggtern::coord_tern(L = "x", T = "y", R = "z") +
      # USDA texture polygons
      ggplot2::geom_polygon(
        mapping = ggplot2::aes(
          fill = label
        ),
        alpha = 0,
        linewidth = 0.5,
        color = "#201F1F",
        show.legend = FALSE
      ) +
      # USDA texture labels
      ggplot2::geom_text(
        data = usdaTexture$labels,
        mapping = ggplot2::aes(
          label = label,
          angle = angle
        ),
        color = "#201F1F",
        size = 1.6
      ) +
      # Theme
      ggplot2::theme_minimal() +
      ggplot2::labs(
        xarrow = "Sand (%)",
        yarrow = "Clay (%)",
        zarrow = "Silt (%)"
      ) +
      # Tweak theme of plot
      ggplot2::theme(
        # Font
        text = ggplot2::element_text(size = 10, family = body_font),
        # Triangle panel
        tern.panel.mask.show = FALSE,
        # Axes
        tern.axis.title.show = FALSE,
        # clay
        tern.axis.text.T = ggplot2::element_text(
          hjust = -0.01
        ),
        # sand
        tern.axis.text.L = ggplot2::element_text(
          hjust = 0.8,
          angle = 60
        ),
        # silt
        tern.axis.text.R = ggplot2::element_text(
          vjust = 0.8,
          angle = -60
        ),
        tern.axis.ticks = ggplot2::element_line(color = "transparent"),
        # Legend
        legend.position = "right",
        legend.text = ggplot2::element_text(size = 10),
        legend.margin = ggplot2::margin(l = -50),
        legend.title = ggplot2::element_blank(),
        # Arrows
        tern.axis.arrow.show = TRUE,
        tern.axis.arrow.sep = 0.1,
        # clay
        tern.axis.arrow.text.T = ggplot2::element_text(vjust = -1),
        # sand
        tern.axis.arrow.text.L = ggplot2::element_text(vjust = -0.6),
        # silt
        tern.axis.arrow.text.R = ggplot2::element_text(vjust = 1)
      )
  })
}
#' Add `ggplot2::geom_point()` points to texture triangle
#'
#' @param df Data frame containing columns for sand, silt, and clay.
#' @param sand,silt,clay Column names in `df` for sand, silt, and clay % values.
#' @param ... Other arguments to pass into `ggplot2::aes()`.
#' @returns `ggplot2` texture triangle with points for all samples.
#' @export
#'
#' @examples
#' # Create a texture triangle with points colored by texture
#' make_texture_triangle(body_font = "sans") +
#'   add_texture_points(
#'     df = exampleData,
#'     sand = `sand_%`,
#'     silt = `silt_%`,
#'     clay = `clay_%`,
#'     color = texture
#'   ) +
#'   ggplot2::scale_color_viridis_d()
#'
#'
#' # Remember these are `ggplot2` functions and require `+` instead of
#' #  pipes (`|>` or `%>%`)
#' try({
#'   make_texture_triangle(body_font = "sans") +
#'     add_texture_points(
#'       df = exampleData,
#'       sand = `sand_%`,
#'       silt = `silt_%`,
#'       clay = `clay_%`,
#'       color = texture
#'     ) |>
#'     ggplot2::scale_color_viridis_d()
#' })
add_texture_points <- function(
    df,
    sand,
    silt,
    clay,
    ...
) {
  suppressWarnings({
    ggplot2::geom_point(
      data = df,
      mapping = ggplot2::aes(
        x = {{ sand }},
        y = {{ clay }},
        z = {{ silt }},
        ...
      )
    )
  })
}
#' Theme for facetted strip plots
#'
#' @inheritParams make_texture_triangle
#' @param strip_color Color of facet strip background. Defaults to WaSHI blue.
#' @param strip_text_color Color of facet strip text. Defaults to white.
#' @param ... Other arguments to pass into `ggplot2::theme()`.
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
#'   subset(measurement_group == "biological")
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
#'   id = sampleId,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
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
#'   id = sampleId,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
#'   size = category,
#'   alpha = category,
#'   shape = category
#' ) |>
#'   set_scales()
theme_facet_strip <- function(
    ...,
    body_font = "Poppins",
    strip_color = washi::washi_pal[["standard"]][["blue"]],
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
#'   green.
#' @param secondary_color Color of sample points with `"Same crop"` or `"Same
#'   county"` values in the `category` column. Defaults to WaSHI gray.
#' @param other_color Color of sample points with `"Other fields"` value in
#'   `category` column. Defaults to WaSHI tan.
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
#'   subset(measurement_group == "biological")
#'
#' # Make strip plot
#'
#' make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sampleId,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
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
#'   id = sampleId,
#'   group = abbr_unit,
#'   tooltip = label
#' ) +
#'   theme_facet_strip(body_font = "sans")
set_scales <- function(
    plot,
    primary_color = washi::washi_pal[["standard"]][["red"]],
    secondary_color = washi::washi_pal[["standard"]][["ltgray"]],
    other_color = washi::washi_pal[["standard"]][["tan"]]
) {
  plot +
    ggplot2::scale_alpha_manual(values = c(
      "Your fields" = 0.8,
      "Same county" = 0.6,
      "Same crop" = 0.6,
      "Other fields" = 0.5
    )) +
    ggplot2::scale_color_manual(values = c(
      "Your fields" = primary_color,
      "Same county" = secondary_color,
      "Same crop" = secondary_color,
      "Other fields" = other_color
    )) +
    ggplot2::scale_shape_manual(values = c(
      "Your fields" = 15,
      "Same county" = 17,
      "Same crop" = 18,
      "Other fields" = 19
    )) +
    ggplot2::scale_size_manual(values = c(
      "Your fields" = 3,
      "Same county" = 2.7,
      "Same crop" = 2.7,
      "Other fields" = 1.7
    ))
}

#' Make a facetted strip plot
#'
#' @param df Data frame to plot.
#' @param x Column for x-axis. For these strip plots, we recommend using a dummy
#'   variable to act as a placeholder. Defaults to a column named `dummy` with
#'   only one value ("dummy") for all rows.
#' @param y Column for y-axis. Defaults to `value`.
#' @param id Column with unique identifiers for each sample to use as `data_id`
#'   for interactive plots. Defaults to `sampleId`.
#' @param group Column to facet by. Defaults to `abbr_unit`.
#' @param tooltip Column with tooltip labels for interactive plots.
#' @inheritParams add_texture_points
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
#'   subset(measurement_group == "biological")
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
#'   id = sampleId,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
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
#' # Example of strip plot with `x` set to the facet group instead of a
#' # dummy variable.
#' make_strip_plot(df_plot_bio, x = abbr_unit) |>
#'   set_scales() +
#'   theme_facet_strip(body_font = "sans")
make_strip_plot <- function(
    df,
    ...,
    x = dummy,
    y = value,
    id = sampleId,
    group = abbr_unit,
    tooltip = label
) {
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
        linetype = "Project Average",
        data_id = {{ group }},
        tooltip = glue::glue("Avg: {round(mean, 2)} {unit}")
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Project Average" = "dashed")
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
#' @inheritParams make_texture_triangle
#' @param width Width of SVG output in inches. Defaults to 6.
#' @param height Height of SVG output in inches. Defaults to 4.
#' @param ... Other arguments to pass into `ggiraph::girafe_options()`.
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
#'   subset(measurement_group == "biological")
#'
#' # NOTE: the plot gets piped into the `set_scales()` function, which gets
#' # added to `theme_facet_strip()`.

#' plot <- make_strip_plot(
#'   df_plot_bio,
#'   x = dummy,
#'   y = value,
#'   id = sampleId,
#'   group = abbr_unit,
#'   tooltip = label,
#'   color = category,
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
        "Can't find font family `{body_font}` on your system.",
        "i" = "Defaulting to a sans-serif font.",
        "i" = "See the {.href [`ggiraph book`](https://www.ardata.fr/ggiraph-book/fonts.html)} for help."
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
