#' Make a texture triangle
#'
#' ```{r child = "man/rmd/wrangle.Rmd"}
#' ````
#'
#' @param df Data frame containing columns: `Sand`, `Clay`, `Silt`, `category`
#' @param primary_color Color of producer's sample points Defaults to WaSHI
#'   green.
#' @param secondary_color Color of sample points with `"Same crop"` or `"Same
#'   county"` values in the `category` column. Defaults to WaSHI gray.
#' @param other_color Color of sample points with `"Other fields"` value in
#'   `category` column. Defaults to WaSHI tan.
#' @param font_family Font family to use throughout plot. Defaults to
#'   `"Poppins"`.
#'
#' @returns `ggplot2` texture triangle.
#'
#' @export
#'
#' @examples
#' # For Poppins font, must have it installed and registered in R with
#' # the `{extrafont}` package.
#' library(extrafont)
#'
#' # Read in wrangled texture data.
#' # See `data_wrangling.R` for processing steps.
#' path <- soils_example("dfTexture.csv")
#' df <- read.csv(path)
#'
#' # The data structure necessary to render the df triangle
#' dplyr::slice_sample(df, n = 1, by = category) |>
#'   dplyr::glimpse()
#'
#' # Make sure class of `category` is `ordered factor` with `Your fields` at the
#' # end so it is plotted on top of the other points.
#' df$category <- factor(
#'   df$category,
#'   levels = c(
#'     "Other fields",
#'     "Same county",
#'     "Same crop",
#'     "Your fields"
#'   ),
#'   ordered = TRUE
#' )
#'
#' class(df$category)
#'
#' levels(df$category)
#'
#' # Make the plot
#' make_texture_triangle(df)
#'
make_texture_triangle <- function(
  df,
  primary_color = washi::washi_pal[["standard"]][["red"]],
  secondary_color = washi::washi_pal[["standard"]][["gray"]],
  other_color = washi::washi_pal[["standard"]][["tan"]],
  font_family = "Poppins"
    ) {
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
        size = 2
      ) +
      ggplot2::geom_point(
        data = df,
        mapping = ggplot2::aes(
          x = Sand,
          y = Clay,
          z = Silt,
          alpha = category,
          color = category,
          shape = category,
          size = category
        )
      ) +
      # Define scales for categories
      ggplot2::scale_alpha_manual(
        values = c(
          "Your fields" = 0.8,
          "Same county" = 0.8,
          "Same crop" = 0.8,
          "Other fields" = 0.6
        )
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Your fields" = primary_color,
          "Same county" = secondary_color,
          "Same crop" = secondary_color,
          "Other fields" = other_color
        )
      ) +
      ggplot2::scale_shape_manual(
        values = c(
          "Your fields" = 15,
          "Same county" = 17,
          "Same crop" = 18,
          "Other fields" = 19
        )
      ) +
      ggplot2::scale_size_manual(
        values = c(
          "Your fields" = 3,
          "Same county" = 2.2,
          "Same crop" = 2.5,
          "Other fields" = 2
        )
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
        text = ggplot2::element_text(size = 12, family = font_family),
        # Triangle panel
        tern.panel.mask.show = FALSE,
        # Axes
        tern.axis.title.show = FALSE,
        tern.axis.text.T = ggplot2::element_text(
          hjust = 0.5
        ),
        tern.axis.text.L = ggplot2::element_text(
          hjust = 0.5,
          angle = 60
        ),
        tern.axis.text.R = ggplot2::element_text(
          vjust = 0.5,
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
        tern.axis.arrow.sep = 0.075,
        tern.axis.arrow.text.T = ggplot2::element_text(vjust = -0.6),
        tern.axis.arrow.text.L = ggplot2::element_text(vjust = -0.6),
        tern.axis.arrow.text.R = ggplot2::element_text(vjust = 1)
      )
  })
}

#' Make facetted strip plot
#'
#' ```{r child = "man/rmd/wrangle.Rmd"}
#' ````
#'
#' @param df Dataframe containing columns: `category`, `abbr_unit`, `unit`,
#'   `dummy`, and `sampleLabel`.
#' @param output Type of ouput: either `"static"` or `"html"`
#' @inheritParams make_texture_triangle
#' @param primary_accent_color Color of facet strip background. Defaults to
#'   WaSHI blue.
#'
#' @export
#' @returns Facetted `ggplot2.` strip plot
#'
#' @examples
#' # For Poppins font, must have it installed and registered in R with
#' # the `{extrafont}` package.
#' library(extrafont)
#'
#' # Read in wrangled plot data.
#' # See `data_wrangling.R` for processing steps.
#' path <- soils_example("dfPlot.csv")
#' df <- read.csv(path, encoding = "UTF-8")
#'
#' # The data structure necessary to render the df triangle
#' dplyr::slice_sample(df, n = 1, by = category) |>
#'   dplyr::glimpse()
#'
#' # Make sure class of `category` is `ordered factor` with `Your fields` at the
#' # end so it is dfted on top of the other points.
#' df$category <- factor(
#'   df$category,
#'   levels = c(
#'     "Other fields",
#'     "Same county",
#'     "Same crop",
#'     "Your fields"
#'   ),
#'   ordered = TRUE
#' )
#'
#' class(df$category)
#'
#' levels(df$category)
#'
#' # Make the plot
#' make_strip_plot(df, output = "static")
#'
make_strip_plot <- function(
  df,
  output,
  font_family = "Poppins",
  primary_color = washi::washi_pal[["standard"]][["red"]],
  secondary_color = washi::washi_pal[["standard"]][["ltgray"]],
  other_color = washi::washi_pal[["standard"]][["tan"]],
  primary_accent_color = washi::washi_pal[["standard"]][["blue"]]
    ) {
  output <- match.arg(arg = output, choices = c("static", "html"))

  # Subset data to just producer for labels
  producer <- df[df$category == "Your fields", ]

  # Set number of columns in facet
  ncol <- ifelse(dplyr::n_distinct(df$abbr_unit) > 6, 4, 3)

  # Find project average for each measurement
  averages <- df |>
    dplyr::group_by(abbr_unit, unit) |>
    dplyr::summarize(
      mean = mean(value, na.rm = TRUE),
      .groups = "keep"
    )

  theme <- ggplot2::theme(
    # Font family
    text = ggplot2::element_text(family = font_family),
    # Gridlines formatting
    panel.grid.major.x = ggplot2::element_blank(),
    # Axis formatting
    axis.title = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    # Facet label formatting
    strip.background = ggplot2::element_rect(
      fill = primary_accent_color
    ),
    strip.text = ggtext::element_markdown(
      color = "white",
      face = "bold",
      size = 12
    ),
    # Legend formatting
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

  # Adjust theme for plotly
  if (output == "html") {
    theme <- theme +
      ggplot2::theme(
        # Panel spacing
        panel.spacing.x = ggplot2::unit(6, "line"),
        panel.spacing.y = ggplot2::unit(30, "line"),
        # Facet label formatting
        strip.text = ggtext::element_markdown(
          margin = ggplot2::margin(0.35, 0, 0.6, 0, "cm")
        ),
        # Legend formatting
        legend.text = ggplot2::element_text(
          size = 11
        )
      )
  }

  plot <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = dummy,
      y = value,
      alpha = category,
      color = category,
      shape = category,
      size = category,
      text = paste(sampleLabel, round(value, 2), unit)
    )
  ) +
    ggplot2::geom_hline(
      data = averages,
      mapping = ggplot2::aes(
        yintercept = mean,
        linetype = "Average"
      )
    ) +
    ggplot2::geom_jitter(
      width = 0.2,
      height = 0,
      na.rm = TRUE
    ) +
    # Define styles for producer's samples versus all samples
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
      "Your fields" = 4,
      "Same county" = 2.2,
      "Same crop" = 2.5,
      "Other fields" = 2
    )) +
    ggplot2::scale_linetype_manual(
      values = c("Average" = "dashed")
    ) +
    ggplot2::facet_wrap(
      ~abbr_unit,
      ncol = ncol,
      scales = "free_y"
    ) +
    # Customize theme of plot
    ggplot2::theme_bw() +
    theme

  # Uncomment if you want to label producer's samples
  #
  # Label fieldId for producer's samples if they have less than 4
  # samples
  #
  # n <- subset(data, category == "Your fields") |>
  #   dplyr::select(sampleId) |>
  #   dplyr::n_distinct()
  #
  # # Two geom_label_repels so the solid text shows above the
  # transparent background
  #
  # if (n < 5 & output == "static") {
  #   plot <- plot + ggrepel::geom_label_repel(
  #     data = producer,
  #     mapping = ggplot2::aes(
  #       label = paste(fieldName, ":", round(value, 2), unit)
  #     ),
  #     alpha = 0.7,
  #     color = NA,
  #     size = 2.8,
  #     hjust = 1,
  #     direction = "y",
  #     max.time = 5,
  #     force = 100,
  #     force_pull = 1,
  #     fontface = "bold",
  #     label.padding = unit(0.15, "lines"),
  #     show.legend = FALSE,
  #     # Don't show line connecting label to point
  #     segment.color = NA,
  #     seed = 12345
  #   ) +
  #     ggrepel::geom_label_repel(
  #       data = producer,
  #       mapping = ggplot2::aes(
  #         label = paste(fieldName, ":", round(value, 2), unit)
  #       ),
  #       alpha = 1,
  #       fill = NA,
  #       color = primary_color,
  #       size = 2.8,
  #       hjust = 1,
  #       direction = "y",
  #       max.time = 5,
  #       force = 100,
  #       force_pull = 1,
  #       fontface = "bold",
  #       label.padding = unit(0.15, "lines"),
  #       show.legend = FALSE,
  #       # Don't show line connecting label to point
  #       segment.color = NA,
  #       seed = 12345
  #     )
  # }

  return(plot)
}

#' Make facetted strip plot interactive with `plotly`
#'
#' @description
#'
#' This function runs `make_strip_plot()` then adds `plotly` interactivity.
#'
#' NOTE: `plotly` has issues with overlapping axis labels when facetting (See
#' this [GitHub issue](https://github.com/plotly/plotly.R/issues/1224)).There
#' were some hacky solutions to getting these plots to look good in the rendered
#' reports.
#'
#' @inheritParams make_strip_plot
#' @export
#' @returns Facetted `plotly` strip plot.
#'
#' @examples
#' # For Poppins font, must have it installed and registered in R with
#' # the `{extrafont}` package.
#' library(extrafont)
#'
#' # Read in wrangled plot data.
#' # See `data_wrangling.R` for processing steps.
#' path <- soils_example("dfPlot.csv")
#' df <- read.csv(path, encoding = "UTF-8")
#'
#' # The data structure necessary to render the df triangle
#' dplyr::slice_sample(df, n = 1, by = category) |>
#'   dplyr::glimpse()
#'
#' # Make sure class of `category` is `ordered factor` with `Your fields`
#' # at the end so it is plotted on top of the other points.
#' df$category <- factor(
#'   df$category,
#'   levels = c(
#'     "Other fields",
#'     "Same county",
#'     "Same crop",
#'     "Your fields"
#'   ),
#'   ordered = TRUE
#' )
#'
#' class(df$category)
#'
#' levels(df$category)
#'
#' # Remember this function creates the plot specifically for the Quarto
#' # rendered reports and will not look right outside of the reports.
#'
#' make_plotly(df)
make_plotly <- function(
  df,
  font_family = "Poppins",
  primary_color = washi::washi_pal[["standard"]][["red"]],
  secondary_color = washi::washi_pal[["standard"]][["ltgray"]],
  other_color = washi::washi_pal[["standard"]][["tan"]],
  primary_accent_color = washi::washi_pal[["standard"]][["blue"]]
    ) {
  # ggplot -> plotly has issues with overlapping axis labels when facetting
  # https://github.com/plotly/plotly.R/issues/1224 possible solution to look
  # into:
  # https://stackoverflow.com/questions/42763280/r-ggplot-and-plotly-axis-margin-wont-change
  # current remedy is adjusting the panel.spacing.x and .y in make_strip_plot
  # function

  df_plotly <- make_strip_plot(
    df,
    output = "html"
  ) |>
    plotly::ggplotly(tooltip = "text") |>
    plotly::layout(
      # Legend title doesn't actually appear.
      # It prevents the variable names from showing up though.
      legend = list(orientation = "h", title = "Interactive Legend"),
      margin = list(t = 100),
      font = list(family = font_family, size = 15)
    ) |>
    plotly::style(hoverlabel = list(font = list(size = 15))) |>
    plotly::config(
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "zoomIn2d",
        "zoomOut2d",
        "select2d",
        "lasso2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "resetScale2d"
      ),
      displaylogo = FALSE,
      displayModeBar = TRUE,
      toImageButtonOptions = list(
        filename = paste0("soil_", deparse(substitute(df)), "_plot")
      )
    )

  # this gets rid of duplicate legend entries
  # modified from SO source: https://stackoverflow.com/a/69035732

  for (i in seq_along(df_plotly$x$data)) {
    # Is the layer the first entry of the group?
    is_first <- stringr::str_detect(df_plotly$x$data[[i]]$name, "\\b1\\b")
    # Extract the group identifier and assign it to the name and
    # legend group arguments
    df_plotly$x$data[[i]]$name <- stringr::str_remove_all(
      df_plotly$x$data[[i]]$name,
      "[:punct:]|[:digit:]|NA"
    )

    df_plotly$x$data[[i]]$legendgroup <- df_plotly$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (!is_first) df_plotly$x$data[[i]]$showlegend <- FALSE
  }

  return(df_plotly)
}

#' `ggplot2::ggsave()` with default arguments
#'
#' Wrapper of `ggplot2::ggsave()` to save a plot to the specified directory with
#' default settings of `.png` output with dimensions of 6" x 4".
#'
#' @inheritParams ggplot2::ggsave
#' @param path Path of the directory to save plot to. File name is determined by
#'   name of plot and file extension.
#'
#' @returns Side effects of saving plot.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' plot <- exampleData |>
#'   ggplot(aes(crop)) +
#'   geom_bar() +
#'   coord_flip()
#'
#' # Create temp file to save plot
#' file <- tempfile()
#'
#' # Save plot
#' save_plot(plot, path = file)
#'
#' # Delete plot file
#' unlink(file)
save_plot <- function(
  plot,
  path,
  device = "png",
  height = 4,
  width = 6,
  units = "in"
    ) {
  if (!dir.exists(path)) {
    dir.create(path)
  }

  ggplot2::ggsave(
    plot = plot,
    filename = paste0(deparse(substitute(plot)), ".", device),
    path = path,
    dpi = 300,
    scale = 1,
    height = height,
    width = width,
    units = units,
    device = device
  )
}
