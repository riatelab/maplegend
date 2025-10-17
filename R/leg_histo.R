#' @title Plot a histogram legend
#' @description This function plots a histogram legend.
#'
#' @param pal a set of colors
#' @param alpha if \code{pal} is a \link{hcl.colors} palette name, the
#' alpha-transparency level in the range \[0,1\]
#' @param col_na color for missing values
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y))
#' @param val histogram parameters
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param val_dec decimal separator
#' @param val_big thousands separator
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param box_border color of the boxes' borders
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param box_cex width and height cex of boxes
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param adj adj
#' @param frame_border border color of the frame
#' @keywords internal
#' @noRd
#' @import graphics
#' @return No return value, a legend is displayed.
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' leg_choro(val = c(1, 2, 3, 4), pal = c("red1", "red3", "red4"))
leg_histo <- function(pos = "left",
                      val,
                      pal = "Inferno",
                      alpha = NULL,
                      title = "Legend Title",
                      title_cex = .8 * size,
                      val_cex = .6 * size,
                      val_rnd = 0,
                      val_dec = ".",
                      val_big = "",
                      col_na = "white",
                      no_data = FALSE,
                      no_data_txt = "No Data",
                      frame = FALSE,
                      frame_border = fg,
                      box_border = "#333333",
                      bg = "#f7f7f7",
                      fg = "#333333",
                      size = 1,
                      box_cex = c(1, 1),
                      return_bbox = FALSE,
                      adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # n bars
  n_bar <- length(val$breaks) - 1

  # bar heights
  hh <- val$density

  # select values for x axis
  bks <- val$breaks
  vleg <- c(min(bks), max(bks))
  pleg <- pretty(vleg, min.n = 3)
  pe <- pleg[2] - pleg[1]
  if ((vleg[1] - pleg[1]) > (pe / 4)) {
    pleg <- pleg[-1]
  }
  if ((pleg[length(pleg)] - vleg[2]) > (pe / 4)) {
    pleg <- pleg[-length(pleg)]
  }
  vmin <- min(vleg, pleg)
  vmax <- max(vleg, pleg)
  vleg <- pleg
  valo <- get_val_rnd(val = bks, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)
  val <- get_val_rnd(val = vleg, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)

  # bar colors
  pal <- get_pal(pal, n_bar, alpha = alpha)

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # graph size
  hist_dim <- list(
    w = box_cex[1] * size * x_spacing * 5 * n_bar,
    h = box_cex[2] * size * y_spacing * 5 * 2
  )

  # label dimension
  r <- strwidth(valo[1], units = "user", cex = val_cex, font = 1) / 2
  s <- strwidth(valo[length(valo)], units = "user", cex = val_cex, font = 1) / 2
  labels_dim <- list(
    w = r + s + hist_dim$w,
    h = max(strheight(val, units = "user", cex = val_cex, font = 1))
  )

  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        labels_dim$w
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      hist_dim$h + y_spacing +
      labels_dim$h +
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

  # display bars
  left_init <- legend_coords$left + x_spacing + r
  bottom <- rep(legend_coords$bottom + y_spacing + labels_dim$h + y_spacing, n_bar)
  top <- bottom + hist_dim$h * hh / max(hh)
  w_bars <- hist_dim$w * ((bks - vmin) / (vmax - vmin))
  left <- w_bars[-length(w_bars)] + left_init
  right <- w_bars[-1] + left_init
  rect(
    xleft = left, ybottom = bottom, xright = right,
    ytop = top, col = pal, border = box_border, lwd = .5
  )

  # display labels
  x <- left_init + hist_dim$w * ((vleg - vmin) / (vmax - vmin))
  y <- rep(bottom[1] - y_spacing, length(vleg))
  text(x = x, y = y, labels = val, cex = val_cex, adj = c(0.5, 1), col = fg)
  segments(
    x0 = x, y0 = bottom, x1 = x, y1 = bottom - y_spacing / 2,
    col = box_border, lwd = .7
  )

  return(invisible(NULL))
}
