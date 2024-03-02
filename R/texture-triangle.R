#' Make a textural class triangle
#'
#' Make a texture triangle with USDA textural classes.
#'
#' @param body_font Font family to use throughout plot. Defaults to `"Poppins"`.
#' @param show_names Boolean. Defaults to `TRUE` to show USDA textural class
#'   names.
#' @param show_lines Boolean. Defaults to `TRUE` to show boundaries of USDA
#'   textural classes.
#' @param show_grid Boolean. Defaults to `FALSE` to hide grid lines at each 10
#'   level of each soil component.
#'
#' @source Adapted from {plotrix}:
#'   <https://github.com/plotrix/plotrix/blob/0d4c2b065e2c2d327358ac8cdc0b0d46b89bea7f/R/soil.texture.R>
#'
#' @returns Opens the graphics device with a triangle plot containing USDA
#'   textural classes.
#' @export
#'
#' @examples
#' # Note the text appears squished in this example since the width, height,
#' # and resolution have been optimized to print the figure 6 in wide in the
#' # report.
#'
#' make_texture_triangle(body_font = "sans")
make_texture_triangle <- function(
  body_font = "Poppins",
  show_names = TRUE,
  show_lines = TRUE,
  show_grid = FALSE
    ) {
  graphics::par(
    xpd = TRUE,
    family = body_font
  )

  triax_plot(
    show_grid = show_grid
  )

  graphics::arrows(0.12, 0.41, 0.22, 0.57, length = 0.15)
  graphics::arrows(0.78, 0.57, 0.88, 0.41, length = 0.15)
  graphics::arrows(0.6, -0.1, 0.38, -0.1, length = 0.15)

  if (show_lines) {
    triax_segments <- function(h1, h3, t1, t3, co = "gray") {
      graphics::segments(
        1 - h1 - h3 / 2,
        h3 * sin(pi / 3),
        1 - t1 - t3 / 2,
        t3 * sin(pi / 3),
        col = "gray"
      )
    }
    # from bottom-left to up
    h1 <- c(85, 70, 80, 52, 52, 50, 20, 8, 52, 45, 45, 65, 45, 20, 20) / 100
    h3 <- c(0, 0, 20, 20, 7, 0, 0, 12, 20, 27, 27, 35, 40, 27, 40) / 100
    t1 <- c(90, 85, 52, 52, 43, 23, 8, 0, 45, 0, 45, 45, 0, 20, 0) / 100
    t3 <- c(10, 15, 20, 7, 7, 27, 12, 12, 27, 27, 55, 35, 40, 40, 60) / 100
    triax_segments(h1, h3, t1, t3, "#140F00")
  }

  if (show_names) {
    xpos <- c(
      0.5,
      0.7,
      0.7,
      0.73,
      0.73,
      0.5,
      0.275,
      0.275,
      0.27,
      0.27,
      0.25,
      0.135,
      0.18,
      0.055,
      0.49,
      0.72,
      0.9
    )

    ypos <- c(
      0.66,
      0.49,
      0.44,
      0.36,
      0.32,
      0.35,
      0.43,
      0.39,
      0.3,
      0.26,
      0.13,
      0.072,
      0.032,
      0.024,
      0.18,
      0.15,
      0.06
    ) * sin(pi / 3)

    snames <- c(
      "Clay",
      "Silty",
      "Clay",
      "Silty Clay",
      "Loam",
      "Clay Loam",
      "Sandy",
      "Clay",
      "Sandy Clay",
      "Loam",
      "Sandy Loam",
      "Loamy",
      "Sand",
      "Sand",
      "Loam",
      "Silt Loam",
      "Silt"
    )
    boxed_labels(xpos, ypos, snames, border = FALSE, xpad = 0.5, bg = "transparent")
  }
}

#' Add points to texture triangle
#'
#' To vary color, symbol, and size of points by a grouping variable, call this
#' function once for each value of the grouping variable. Add layers from bottom
#' to top. The below example adds the red points last so they are plotted on top
#' of the gray points.
#'
#' @param texture_df Data frame or matrix where each row is a soil sample and
#'   three numeric columns contain sand, silt, and clay percentages or
#'   proportions. The order of sand, silt, clay is required for correct
#'   plotting.
#' @param color Color of the points. Defaults to WaSHI red.
#' @param pch Numeric value of plotting symbol. See [`graphics::points()`] for
#'   options and details. Defaults to 19, which is a filled-in circle.
#' @param size Numeric expansion factor for points. Defaults to 1.5.
#' @param ... Other arguments passed to [`graphics::points()`].
#'
#' @source Adapted from {plotrix}:
#'   <https://github.com/plotrix/plotrix/blob/0d4c2b065e2c2d327358ac8cdc0b0d46b89bea7f/R/soil.texture.R>
#'
#' @returns A list of x, y coordinates of the soil textures plotted.
#' @export
#'
#' @examples
#' texture <- soils::washi_data |>
#'   dplyr::select(
#'     sand = sand_percent,
#'     silt = silt_percent,
#'     clay = clay_percent
#'   )
#'
#' make_texture_triangle(body_font = "sans")
#'
#' # Add gray points
#' add_texture_points(
#'   tail(texture, 5),
#'   color = "#3E3D3D90",
#'   pch = 19
#' )
#'
#' # Add red points
#' add_texture_points(
#'   head(texture, 5),
#'   color = "#a60f2dCC",
#'   pch = 15
#' )
#'
#' # Note the text appears squished in this example since the width, height,
#' # and resolution have been optimized to print the figure 6 in wide in the
#' # report.
add_texture_points <- function(
  texture_df = NULL,
  color = "#a60f2dCC",
  pch = 19,
  size = 1.5,
  ...
    ) {
  graphics::par(xpd = FALSE)

  texture_points <- triax_points(
    texture_df,
    color = color,
    pch = pch,
    size = size,
    ...
  )
  invisible(texture_points)
}

#' Add a legend to the texture triangle
#'
#' @param x,y X and Y coordinates used to position the legend. Location may also
#'   be specified by setting `x` to a single keyword from the list
#'   `"bottomright"`, `"bottom"`, `"bottomleft"`, `"left"`, `"topleft"`,
#'   `"top"`, `"topright"`, `"right"`, and `"center"`.
#' @param box Boolean. `TRUE` to draw a box around the legend. Defaults to `FALSE`
#'   for no box.
#' @param legend Character vector to appear in legend.
#' @param color Character vector of the color of the points.
#' @param pch Numeric vector of plotting symbols. See [`graphics::points()`] for
#'   options and details.
#' @param size Numeric expansion factor for points.
#' @param vertical_spacing Numeric spacing factor for vertical line distances
#'   between each legend item.
#' @param ... Other arguments passed to [`graphics::legend()`].
#' @returns A list with list components for the legend's box and legend's
#'   text(s).
#'
#' @export
#'
#' @examples
#' texture <- washi_data |>
#'   dplyr::select(
#'     sand = sand_percent,
#'     silt = silt_percent,
#'     clay = clay_percent
#'   )
#'
#' make_texture_triangle(body_font = "sans")
#'
#' # Add gray points
#' add_texture_points(
#'   tail(texture, 5),
#'   color = "#3E3D3D90",
#'   pch = 19
#' )
#'
#' # Add red points
#' add_texture_points(
#'   head(texture, 5),
#'   color = "#a60f2dCC",
#'   pch = 15
#' )
#'
#' # Add legend
#' add_legend(
#'   legend = c("Red squares", "Gray circles"),
#'   color = c("#a60f2dCC", "#3E3D3D90"),
#'   pch = c(15, 19),
#'   vertical_spacing = 2
#' )
#'
#' # Note the text appears squished in this example since the width, height,
#' # and resolution have been optimized to print the figure 6 in wide in the
#' # report.
add_legend <- function(
  x = 1,
  y = 0.7,
  box = FALSE,
  legend = c(
    "Your fields",
    "Same county",
    "Same crop",
    "Other fields"
  ),
  color = c(
    "#a60f2dCC",
    "#3E3D3D99",
    "#3E3D3D99",
    "#ccc29c80"
  ),
  pch = c(15, 17, 18, 19),
  size = c(2.4, 2.16, 2.16, 1.36),
  vertical_spacing = 1.5,
  ...
    ) {
  graphics::par(xpd = NA)

  # Box is drawn if bty = "o". Box is not drawn if bty = "n".
  bty <- ifelse(box, "o", "n")

  legend(
    x = x,
    y = y,
    bty = bty,
    legend = legend,
    col = color,
    pch = pch,
    pt.cex = size,
    y.intersp = vertical_spacing,
    ...
  )
}

# Internal functions for use in texture triangle functions =====================

# Adapted from {plotrix}:
# https://github.com/plotrix/plotrix/blob/0d4c2b065e2c2d327358ac8cdc0b0d46b89bea7f/R/triax_R.

triax_points <- function(
  x,
  color,
  pch,
  size,
  ...
    ) {
  df <- x

  x <- x[mapply(is.numeric, x)]

  if (grDevices::dev.cur() == 1) {
    stop("Cannot add points unless the triax_frame has been drawn")
  }
  if (missing(x)) {
    stop("Usage: triax_points(x,...)\n\twhere x is a 3 column array of proportions or percentages")
  }
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("x must be a matrix or data frame with at least 3 columns and one row.")
  }
  if (any(x > 1) || any(x < 0)) {
    if (any(x < 0)) {
      stop("All proportions must be between zero and one.")
    }
    if (any(x > 100)) {
      stop("All percentages must be between zero and 100.")
    }
    # convert percentages to proportions
    x <- x / 100
  }
  if (any(abs(rowSums(x) - 1) > 0.01)) {
    warning("At least one set of proportions does not equal one.")
  }
  sin60 <- sin(pi / 3)

  ypos <- x[, 3] * sin60
  xpos <- 1 - (x[, 1] + x[, 3] * 0.5)

  graphics::points(
    x = xpos,
    y = ypos,
    pch = pch,
    col = color,
    cex = size,
    type = "p",
    ...
  )
  invisible(list(x = xpos, y = ypos))
}

triax_frame <- function(
  at = seq(0.1, 0.9, by = 0.1),
  axis_labels = c("Sand (%)", "Silt (%)", "Clay (%)"),
  tick_labels = list(
    l = seq(10, 90, by = 10),
    r = seq(10, 90, by = 10),
    b = seq(10, 90, by = 10)
  ),
  col_axis = "#140F00",
  cex_axis = 1,
  cex_ticks = 1,
  align_labels = TRUE,
  show_grid = FALSE,
  col_grid = "gray",
  lty_grid = 3,
  ...
    ) {
  sin60 <- sin(pi / 3)

  # bottom ticks
  bx1 <- at
  bx2 <- bx1 + 0.01 - 0.02
  by1 <- rep(0, 9)
  by2 <- rep(-0.02 * sin60, 9)

  # left ticks
  ly1 <- at * sin60
  lx1 <- bx1 * 0.5
  lx2 <- lx1 - 0.02 + 0.013
  ly2 <- ly1

  # right ticks
  rx1 <- at * 0.5 + 0.5
  rx2 <- rx1 + 0.01
  ry1 <- rev(ly1)
  ry2 <- rev(ly2) + 0.02 * sin60

  if (show_grid) {
    graphics::par(fg = col_grid)
    graphics::segments(bx1, by1, lx1, ly1, lty = lty_grid)
    graphics::segments(lx1, ly1, rev(rx1), rev(ry1), lty = lty_grid)
    graphics::segments(rx1, ry1, bx1, by1, lty = lty_grid)
  }

  graphics::par(fg = col_axis, xpd = TRUE)
  if (is.null(tick_labels)) {
    tick_labels <- list(l = at, r = at, b = at)
  }

  # left axis label
  if (align_labels) graphics::par(srt = 60)
  graphics::text(0.13, 0.5, axis_labels[3], adj = 0.5, cex = cex_axis)

  # left axis tick labels
  graphics::par(srt = 0)
  xoffset <- 0.05
  yoffset <- 0
  graphics::text(lx1 - xoffset, ly1 + yoffset, tick_labels$l, cex = cex_ticks)

  # right axis label
  if (align_labels) {
    graphics::par(srt = 300)
    label.adj <- 0.5
  } else {
    graphics::par(srt = 0)
    label.adj <- 0
  }

  graphics::text(0.86, 0.52, axis_labels[2], adj = label.adj, cex = cex_axis)

  # right axis tick labels
  graphics::par(srt = 60)
  xoffset <- 0.015
  yoffset <- 0.045
  graphics::text(rx2 + xoffset, ry1 + yoffset, tick_labels$r, cex = cex_ticks)

  # bottom axis tick labels
  graphics::par(srt = 300)
  xoffset <- 0.03
  graphics::text(bx1 + xoffset, by1 - 0.05, rev(tick_labels$b), cex = cex_ticks)

  # bottom axis label
  graphics::par(srt = 0)
  graphics::text(0.5, -0.14, axis_labels[1], cex = cex_axis)

  # draw the triangle and ticks
  x1 <- c(0, 0, 0.5)
  x2 <- c(1, 0.5, 1)
  y1 <- c(0, 0, sin60)
  y2 <- c(0, sin60, 0)

  graphics::par(fg = col_axis)

  graphics::segments(x1, y1, x2, y2)
  # bottom ticks
  graphics::segments(bx1, by1, bx2, by2)
  # left ticks
  graphics::segments(lx1, ly1, lx2, ly2)
  # right ticks
  graphics::segments(rx1, ry1, rx2, ry2)
}

triax_plot <- function(show_grid = FALSE) {
  oldpar <- graphics::par("fg", "pty", "mar", "srt", "xpd")

  graphics::par(xpd = TRUE, pty = "s")

  graphics::plot(
    x = 0.5,
    y = 0.5,
    type = "n",
    axes = FALSE,
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = "",
    ylab = ""
  )

  triax_frame(show_grid = show_grid)
}

boxed_labels <- function(
  x,
  y = NULL,
  labels,
  bg = "transparent",
  border = FALSE,
  xpad = 1.2,
  ypad = 1.2,
  srt = 0,
  cex = 0.8,
  adj = 0.5,
  xlog = FALSE,
  ylog = FALSE,
  ...
    ) {
  oldpars <- graphics::par(c("cex", "xpd"))

  graphics::par(cex = cex, xpd = TRUE, fg = "#140F00")

  if (is.null(y) && is.list(x)) {
    y <- unlist(x[[2]])
    x <- unlist(x[[1]])
  }

  box_adj <- adj + (xpad - 1) * cex * (0.5 - adj)

  if (srt == 90 || srt == 270) {
    bheights <- graphics::strwidth(labels)
    theights <- bheights * (1 - box_adj)
    bheights <- bheights * box_adj
    lwidths <- rwidths <- graphics::strheight(labels) * 0.5
  } else {
    lwidths <- graphics::strwidth(labels)
    rwidths <- lwidths * (1 - box_adj)
    lwidths <- lwidths * box_adj
    bheights <- theights <- graphics::strheight(labels) * 0.5
  }

  args <- list(
    x = x,
    y = y,
    labels = labels,
    srt = srt,
    adj = adj,
    col = ifelse(
      colSums(grDevices::col2rgb(bg) * c(1, 1.4, 0.6)) <
        350,
      "white",
      "black"
    )
  )

  args <- utils::modifyList(args, list(...))

  if (xlog) {
    xpad <- xpad * 2
    xr <- exp(log(x) - lwidths * xpad)
    xl <- exp(log(x) + lwidths * xpad)
  } else {
    xr <- x - lwidths * xpad
    xl <- x + lwidths * xpad
  }

  if (ylog) {
    ypad <- ypad * 2
    yb <- exp(log(y) - bheights * ypad)
    yt <- exp(log(y) + theights * ypad)
  } else {
    yb <- y - bheights * ypad
    yt <- y + theights * ypad
  }

  graphics::rect(xr, yb, xl, yt, col = bg, lwd = 0, border = border)
  do.call(graphics::text, args)
  graphics::par(oldpars)
}
