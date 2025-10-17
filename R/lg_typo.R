#' Plot a legend for a typology map
#' @description This function plots a legend for a typology map.
#'
#' @param pal a set of colors
#' @param alpha if \code{pal} is a \link{hcl.colors} palette name, the
#' alpha-transparency level in the range \[0,1\]
#' @param col_na color for missing values
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param val vector of categories.
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param box_border color of the boxes' borders
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param box_cex width and height cex of boxes
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param frame_border border color of the frame
#' @param adj adj
#' @keywords internal
#' @noRd
#' @import graphics
#' @return No return value, a legend is displayed.
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' leg_typo(val = c("type A", "type B"), pal = c("navy", "tomato"))
leg_typo <- function(pos = "topright",
                     val,
                     pal = "Inferno",
                     alpha = NULL,
                     title = "Legend Title",
                     title_cex = .8 * size,
                     val_cex = .6 * size,
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

  # boxes sizes
  w_box <- box_cex[1] * size * x_spacing * 5
  h_box <- box_cex[2] * size * y_spacing * 5 * 2 / 3

  # Nb. boxes and values
  n_val <- n_box <- length(val)

  # box colors
  pal <- get_pal(pal, n_box, alpha = alpha)

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # boxes dimensions
  boxes_dim <- list(w = w_box, h = n_box * h_box + y_spacing * (n_box - 1))

  # label dimension
  labels_dim <- list(
    w = max(strwidth(val, units = "user", cex = val_cex, font = 1))
  )

  # label (+) box dim
  max_sizes <- pmax(
    strheight(val, units = "user", cex = val_cex, font = 1),
    rep(h_box, n_box)
  )

  # NA box and label dimensions
  if (isTRUE(no_data)) {
    na_box_dim <- list(w = w_box, h = h_box)
    na_label_dim <- list(
      w = strwidth(no_data_txt, units = "user", cex = val_cex, font = 1),
      h = max(strheight(no_data_txt, units = "user", cex = val_cex, font = 1), h_box)
    )
  } else {
    na_box_dim <- list(w = 0, h = 0)
    na_label_dim <- list(w = 0, h = 0)
    no_data_txt <- ""
  }

  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        boxes_dim$w + labels_dim$w + x_spacing,
        na_box_dim$w + na_label_dim$w + x_spacing
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      sum(max_sizes) + (n_box - 1) * y_spacing +
      ifelse(na_label_dim$h != 0, na_label_dim$h + y_spacing * size, 0) +
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

  # display boxes
  center <- rep(NA, n_val)
  center[1] <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    max_sizes[1] / 2
  for (i in 2:n_val) {
    center[i] <- center[i - 1] - max_sizes[i - 1] / 2 - y_spacing - max_sizes[i] / 2
  }
  left <- rep(legend_coords$left + x_spacing, n_box)
  right <- left + w_box
  top <- center + h_box / 2
  bottom <- top - h_box
  rect(
    xleft = left, ybottom = bottom, xright = right, ytop = top,
    col = pal, border = box_border, lwd = .7
  )

  # display labels
  x <- rep(legend_coords$left + x_spacing + w_box + x_spacing, n_val)
  y <- center
  text(x = x, y = y, labels = val, cex = val_cex, adj = c(0, 0.5), col = fg)

  if (isTRUE(no_data)) {
    # display na box
    left <- legend_coords$left + x_spacing
    right <- left + w_box
    bottom <- legend_coords$bottom + y_spacing + (na_label_dim$h - na_box_dim$h) / 2
    top <- bottom + h_box
    rect(
      xleft = left, ybottom = bottom, xright = right, ytop = top,
      col = col_na, border = box_border, lwd = .7
    )

    # display na label
    x <- legend_coords$left + x_spacing + w_box + x_spacing
    y <- bottom + (top - bottom) / 2
    text(x = x, y = y, labels = no_data_txt, cex = val_cex, adj = c(0, 0.5), col = fg)
  }
  return(invisible(NULL))
}
