#' Create a texture triangle
#'
#' This plot requires the functions in 'prepare_data.R' to be run
#' first.
#'
#' @param df Dataframe containing columns: 'Sand', 'Clay', 'Silt',
#'   'category'
#' @param primary_color Color of producer's sample points Defaults to
#'   WaSHI green.
#' @param secondary_color Color of sample points with 'sameCrop' or
#'   sameCounty' categories. Defaults to WaSHI gray.
#' @param other_color Color of all other sample points in project.
#'   Defaults to WaSHI tan.
#' @param font_family Font family to use throughout plot. Defaults to
#'   "Poppins".
#'
#' @returns `ggplot` texture triangle.
#'
#' @export
#'

texture_triangle <- function(
    df,
    primary_color = washi::washi_pal[["standard"]][["green"]],
    secondary_color = washi::washi_pal[["standard"]][["gray"]],
    other_color = washi::washi_pal[["standard"]][["tan"]],
    font_family = "Poppins") {
  ggplot2::ggplot(
    data = usda_texture$polygons,
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
      data = usda_texture$labels,
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
}
