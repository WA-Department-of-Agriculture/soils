#' make facetted strip plot
#'
#' @param data dataframe
#'
#' @param output type of ouput, either "static" or "html"
#'
#' @export
#'

make_strip_plot <- function(data,
                            output,
                            primary_color = washi_pal$green,
                            secondary_color = washi_pal$gray,
                            other_color = washi_pal$tan,
                            primary_accent_color = washi_pal$blue) {
  box::use(
    ggplot2[
      theme, element_blank, element_rect, element_text,
      unit, ggplot, aes, geom_hline, geom_jitter,
      scale_linetype_manual, scale_alpha_manual,
      scale_color_manual, scale_size_manual,
      scale_shape_manual, theme_bw, facet_wrap, margin
    ],
    ggtext[element_markdown],
    dplyr[select, n_distinct, summarize],
    ggrepel[geom_label_repel]
  )

  # subset data to just producer for labels
  producer <- data[data$category == "Your fields", ]

  # set number of columns in facet
  ncol <- ifelse(n_distinct(data$abbr_unit) > 6, 4, 3)

  # find project average for each measurement
  averages <- data |>
    group_by(abbr_unit, unit) |>
    summarize(
      mean = mean(value, na.rm = TRUE),
      .groups = "keep"
    )

  # set theme for pdf
  if (output == "static") {
    theme <- theme(
      # font family
      text = element_text(family = "Poppins"),
      # gridlines formatting
      panel.grid.major.x = element_blank(),
      # axis formatting
      axis.title = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      # facet label formatting
      strip.background = element_rect(fill = primary_accent_color),
      strip.text = element_markdown(
        color = "white",
        face = "bold",
        size = 12
      ),
      # legend formatting
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  }

  # set theme for html

  if (output == "html") {
    theme <- theme(
      # font family
      text = element_text(family = "Poppins"),
      # gridlines formatting
      panel.grid.major.x = element_blank(),
      # panel spacing
      panel.spacing.x = unit(6, "line"),
      panel.spacing.y = unit(30, "line"),
      # axis formatting
      axis.title = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      # facet label formatting
      strip.background = element_rect(fill = primary_accent_color),
      strip.text = element_markdown(
        color = "white",
        face = "bold",
        size = 12,
        margin = margin(0.35, 0, 0.6, 0, "cm")
      ),
      # legend formatting
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  }

  plot <- ggplot(
    data = data,
    mapping = aes(
      x = dummy,
      y = value,
      alpha = category,
      color = category,
      shape = category,
      size = category,
      text = paste(sampleLabel, round(value, 2), unit)
    )
  ) +
    geom_hline(
      data = averages,
      mapping = aes(
        yintercept = mean,
        linetype = "Average"
      )
    ) +
    geom_jitter(
      width = 0.2,
      height = 0,
      na.rm = TRUE
    ) +
    # define styles for producer's samples versus all samples
    scale_alpha_manual(values = c(
      "Your fields" = 0.8,
      "Same county" = 0.8,
      "Same crop" = 0.8,
      "All fields" = 0.6
    )) +
    scale_color_manual(values = c(
      "Your fields" = primary_color,
      "Same county" = secondary_color,
      "Same crop" = secondary_color,
      "All fields" = other_color
    )) +
    scale_shape_manual(values = c(
      "Your fields" = 15,
      "Same county" = 17,
      "Same crop" = 18,
      "All fields" = 19
    )) +
    scale_size_manual(values = c(
      "Your fields" = 3,
      "Same county" = 2.2,
      "Same crop" = 2.5,
      "All fields" = 2
    )) +
    scale_linetype_manual(
      values = c("Average" = "dashed")
    ) +
    facet_wrap(
      ~abbr_unit,
      ncol = ncol,
      scales = "free_y"
    ) +
    # customize theme of plot
    theme_bw() +
    theme

  # label fieldId for producer's samples if they have less than 4 samples
  n <- subset(data, category == "Your fields") |>
    select(sampleId) |>
    n_distinct()

  # two geom_label_repels so the solid text shows above the transparent background
  if (n < 5 & output == "static") {
    plot <- plot + geom_label_repel(
      data = producer,
      mapping = aes(
        label = paste(fieldName, ":", round(value, 2), unit)
      ),
      alpha = 0.7,
      color = NA,
      size = 2.8,
      hjust = 1,
      direction = "y",
      max.time = 5,
      force = 100,
      force_pull = 1,
      fontface = "bold",
      label.padding = unit(0.15, "lines"),
      show.legend = FALSE,
      # don't show line connecting label to point
      segment.color = NA,
      seed = 12345
    ) +
      geom_label_repel(
        data = producer,
        mapping = aes(
          label = paste(fieldName, ":", round(value, 2), unit)
        ),
        alpha = 1,
        fill = NA,
        color = primary_color,
        size = 2.8,
        hjust = 1,
        direction = "y",
        max.time = 5,
        force = 100,
        force_pull = 1,
        fontface = "bold",
        label.padding = unit(0.15, "lines"),
        show.legend = FALSE,
        # don't show line connecting label to point
        segment.color = NA,
        seed = 12345
      )
  }

  return(plot)
}

#' make plotly
#'
#' @param data dataframe
#'
#' @export
#'

make_plotly <- function(data,
                        primary_color = washi_pal$green,
                        secondary_color = washi_pal$gray,
                        other_color = washi_pal$tan,
                        primary_accent_color = washi_pal$blue) {

  # ggplot -> plotly has issues with overlapping axis labels when facetting
  # https://github.com/plotly/plotly.R/issues/1224
  # possible solution to look into:
  # https://stackoverflow.com/questions/42763280/r-ggplot-and-plotly-axis-margin-wont-change
  # current remedy is adjusting the panel.spacing.x and .y in make_strip_plot function

  box::use(
    plotly[ggplotly, add_annotations, layout, style, config],
    glue[glue],
    stringr[str_detect, str_remove_all]
  )

  df_plotly <- make_strip_plot(data,
    output = "html",
    primary_color,
    secondary_color,
    other_color,
    primary_accent_color
  ) |>
    ggplotly(tooltip = "text") |>
    layout(
      legend = list(orientation = "h", title = "Interactive Legend"),
      margin = list(t = 100),
      font = list(family = "Poppins", size = 15)
    ) |>
    style(hoverlabel = list(font = list(size = 15))) |>
    config(
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
        filename = glue("soil_{deparse(substitute(data))}_plot")
      )
    )

  # this gets rid of duplicate legend entries
  # modified from SO source: https://stackoverflow.com/a/69035732

  for (i in seq_along(df_plotly$x$data)) {
    # Is the layer the first entry of the group?
    is_first <- str_detect(df_plotly$x$data[[i]]$name, "\\b1\\b")
    # Extract the group identifier and assign it to the name and legend group arguments
    df_plotly$x$data[[i]]$name <- str_remove_all(
      df_plotly$x$data[[i]]$name,
      "[:punct:]|[:digit:]|NA"
    )

    df_plotly$x$data[[i]]$legendgroup <- df_plotly$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (!is_first) df_plotly$x$data[[i]]$showlegend <- FALSE
  }

  return(df_plotly)
}

#' save plot
#'
#' @param plot name of plot to save
#'
#' @param ext file extension type ("png", "svg", etc)
#'
#' @param height height of plot
#'
#' @export
#'

save_plot <- function(plot, ext, height, width) {
  box::use(
    ggplot2[ggsave],
    here[here]
  )
  ggsave(
    plot = plot,
    filename = paste0(deparse(substitute(plot)), ".", ext),
    path = paste0(here(), "/qmd/images/"),
    scale = 1,
    height = height,
    width = width,
    device = ext
  )
}
