#' Texture triangle
#'
#' @param data dataframe with columns: category, Clay, Sand, Silt
#'
#' @export
#'

texture_triangle <- function(data,
                             primary_color = washi_pal$green,
                             secondary_color = washi_pal$gray,
                             other_color = washi_pal$tan) {
  box::use(
    here[here],
    plyr[ddply],
    dplyr[n_distinct, select],
    ggplot2[
      ggplot, aes, geom_polygon, geom_text, geom_point, geom_label,
      scale_alpha_manual, scale_color_manual, scale_size_manual,
      scale_shape_manual, theme_bw, labs, theme, margin, element_blank,
      element_text
    ],
    ggtern[coord_tern, theme_hidetitles, theme_showarrows]
  )
  # get USDA textural classification polygons and labels

  load(paste0(here(), "/data/USDA.RData"))

  # turns Loamy Sand to the side to fit within the polygon

  USDA_label <- ddply(USDA, "Label", function(df) {
    label <- as.character(df$Label[1])
    df$Angle <- switch(label,
      "Loamy Sand" = -35,
      0
    )
    colMeans(df[setdiff(colnames(df), "Label")])
  })

  # create texture triangle

  triangle <- ggplot(
    data = USDA,
    mapping = aes(
      x = Sand,
      y = Clay,
      z = Silt
    )
  ) +
    coord_tern(L = "x", T = "y", R = "z") +
    # USDA texture polygons
    geom_polygon(
      mapping = aes(
        fill = Label
      ),
      alpha = 0,
      size = 0.5,
      color = "#201F1F",
      show.legend = FALSE
    ) +
    # USDA texture labels
    geom_text(
      data = USDA_label,
      mapping = aes(
        label = Label,
        angle = Angle
      ),
      color = "#201F1F",
      size = 2
    ) +
    # sample points that are not the producer's
    geom_point(
      data = data,
      mapping = aes(
        x = Sand,
        y = Clay,
        z = Silt,
        alpha = category,
        color = category,
        shape = category,
        size = category
      )
    ) +
    # define styles for categories
    scale_alpha_manual(
      values = c(
        "Your fields" = 0.8,
        "Same county" = 0.8,
        "Same crop" = 0.8,
        "All fields" = 0.6
      )
    ) +
    scale_color_manual(
      values = c(
        "Your fields" = primary_color,
        "Same county" = secondary_color,
        "Same crop" = secondary_color,
        "All fields" = other_color
      )
    ) +
    scale_shape_manual(
      values = c(
        "Your fields" = 15,
        "Same county" = 17,
        "Same crop" = 18,
        "All fields" = 19
      )
    ) +
    scale_size_manual(
      values = c(
        "Your fields" = 3,
        "Same county" = 2.2,
        "Same crop" = 2.5,
        "All fields" = 2
      )
    ) +
    # show arrows with custom label
    theme_bw() +
    theme_showarrows() +
    labs(
      xarrow = "Sand (%)",
      yarrow = "Clay (%)",
      zarrow = "Silt (%)"
    ) +
    # hide redundant sand, clay, silt titles that show at triangle points
    theme_hidetitles() +
    # customize theme of plot
    theme(
      # font family
      text = element_text(size = 12, family = "Poppins"),
      # legend
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.margin = margin(l = -50),
      legend.title = element_blank(),
      # triangle panel
      tern.panel.mask.show = FALSE
    )

  return(triangle)
}
