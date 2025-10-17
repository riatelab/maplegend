#' Plot a legend for a proportional lines map
#' @description This function plots a legend for proportional lines.
#'
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param lwd width of the largest line
#' @param val vector of values (at least min and max).
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param val_dec decimal separator
#' @param val_big thousands separator
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param size size of the legend; 2 means two times bigger
#' @param col color of the lines
#' @param alpha opacity, in the range [0,1]
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param box_cex width and height cex of boxes
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param frame_border border color of the frame
#' @param adj adj
#' @keywords internal
#' @noRd
#' @return No return value, a legend is displayed.
#' @import graphics
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' leg_prop_line(lwd = 20, val = c(5, 10, 50, 100))
leg_prop_line <- function(pos = "left",
                          val,
                          lwd = .7,
                          col = "tomato4",
                          alpha = NULL,
                          title = "Legend Title",
                          title_cex = .8 * size,
                          val_cex = .6 * size,
                          val_rnd = 0,
                          val_dec = ".",
                          val_big = "",
                          frame = FALSE,
                          frame_border = fg,
                          bg = "#f7f7f7",
                          fg = "#333333",
                          size = 1,
                          box_cex = c(1, 1),
                          return_bbox = FALSE,
                          adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # color mgmt
  col <- ifelse(!is.null(alpha), get_hex_pal(col, alpha), col)

  # boxes sizes
  w_box <- box_cex[1] * size * x_spacing * 5

  # values & values labels
  val <- unique(sort(val, decreasing = TRUE))
  valleg <- get_val_rnd(val = val, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)

  # Nb. boxes and values
  n_val <- n_box <- length(val)

  # symbol sizes
  lwds <- lwd * val / max(val)

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # boxes dimensions
  boxes_dim <- list(w = w_box)

  # label dimension
  labels_dim <- list(
    w = max(strwidth(valleg, units = "user", cex = val_cex, font = 1))
  )

  # label (+) box dim
  max_sizes <- pmax(
    strheight(valleg, units = "user", cex = val_cex, font = 1),
    yinch(lwds / 96)
  )

  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        boxes_dim$w + labels_dim$w + x_spacing
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      sum(max_sizes) + (n_box - 1) * y_spacing +
      y_spacing
  )

  # get legend coordinates
  legend_coords <- get_legend_coords(
    pos = pos, legend_dim = legend_dim,
    adj = adj, frame = frame,
    x_spacing = x_spacing,
    y_spacing = y_spacing
  )

  # return legend coordinates only
  if (return_bbox) {
    return(invisible(legend_coords))
  }

  # display frame
  plot_frame(
    frame = frame, legend_coords = legend_coords,
    bg = bg, frame_border = frame_border,
    x_spacing = x_spacing, y_spacing = y_spacing
  )

  # display title
  plot_title(
    title = title, title_cex = title_cex, title_dim = title_dim,
    fg = fg, legend_coords = legend_coords,
    x_spacing = x_spacing, y_spacing = y_spacing
  )

  # display lines
  center <- rep(NA, n_val)
  center[1] <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    max_sizes[1] / 2
  for (i in 2:n_val) {
    center[i] <- center[i - 1] - max_sizes[i - 1] / 2 - y_spacing - max_sizes[i] / 2
  }
  left <- rep(legend_coords$left + x_spacing, n_box)
  right <- left + w_box
  segments(
    x0 = left,
    y0 = center,
    x1 = right,
    y1 = center,
    col = col,
    lwd = lwds,
    lend = 1
  )

  # display labels
  x <- rep(legend_coords$left + x_spacing + w_box + x_spacing, n_val)
  y <- center
  text(x = x, y = y, labels = valleg, cex = val_cex, adj = c(0, 0.5), col = fg)

  return(invisible(NULL))
}
