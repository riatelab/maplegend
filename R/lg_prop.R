#' Plot a legend for a proportional symbols map
#' @description This function plots a legend for proportional symbols.
#' @param symbol type of symbols, 'circle' or 'square'
#' @param inches size of the biggest symbol (radius for circles, half width
#' for squares) in inches
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param val vector of values (at least min and max)
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param val_dec decimal separator
#' @param val_big thousands separator
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param border color of the symbols borders
#' @param size size of the legend; 2 means two times bigger
#' @param lwd width of the symbols borders
#' @param col color of the symbols (for "prop") or color of the lines (for
#' "prop_line" and "grad_line")
#' @param alpha opacity, in the range [0,1]
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param self_adjust if TRUE values are self-adjusted to keep min, max and
#' intermediate rounded values
#' @param box_cex do nothing
#' @param adj adj
#' @param frame_border border color of the frame
#' @keywords internal
#' @noRd
#' @return No return value, a legend is displayed.
#' @import graphics
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' leg_prop(val = c(1, 20, 100), col = "red", inches = .3)
leg_prop <- function(pos = "left",
                     val,
                     col = "tomato4",
                     alpha = NULL,
                     inches = .3,
                     symbol = "circle",
                     border = "#333333",
                     lwd = .7,
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
                     self_adjust = FALSE,
                     return_bbox = FALSE,
                     box_cex,
                     adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # color mgmt
  col <- ifelse(!is.null(alpha), get_hex_pal(col, alpha), col)

  # values & values labels
  val <- unique(val)
  if (self_adjust == TRUE) {
    val <- self_adjust_v(val, inches, val_cex)
  }
  val <- sort(val, decreasing = TRUE)
  n_val <- length(val)
  valleg <- get_val_rnd(val = val, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # largest symbol size
  symb_sizes <- list(inches = sqrt(val * inches * inches / max(val)))
  symb_sizes$x <- xinch(symb_sizes$inches)
  symb_sizes$y <- yinch(symb_sizes$inches)
  symb_dim <- list(w = xinch(inches * 2), h = yinch(inches * 2))

  # label dimension
  labels_dim <- list(
    w = max(strwidth(valleg, units = "user", cex = val_cex, font = 1)),
    h_top = strheight(valleg[1], units = "user", cex = val_cex, font = 1) / 2,
    h_bottom = strheight(valleg[n_val], units = "user", cex = val_cex, font = 1) / 2
  )

  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        symb_dim$w + labels_dim$w + 2 * x_spacing
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      labels_dim$h_top + symb_dim$h + max(0, labels_dim$h_bottom - symb_sizes$y[n_val] * 2) +
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

  # display symbols & lines
  pal <- rep(NA, n_val)
  pal[1] <- col
  x <- rep(legend_coords$left + x_spacing + symb_sizes$x[1], n_val)
  y <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    labels_dim$h_top - symb_sizes$y[1] * 2 + symb_sizes$y
  if (symbol == "circle") {
    symbols(
      x = x,
      y = y,
      circles = symb_sizes$inches,
      bg = pal,
      fg = border,
      lwd = lwd,
      add = TRUE,
      inches = inches,
    )
    # display lines
    segments(
      x0 = x,
      x1 = x + x_spacing + symb_sizes$x[1],
      y0 = y + symb_sizes$y,
      y1 = y + symb_sizes$y,
      col = border
    )
    # display labels
    text(
      x = x + x_spacing + symb_sizes$x[1] + x_spacing,
      y = y + symb_sizes$y,
      labels = valleg, cex = val_cex, adj = c(0, 0.5), col = fg
    )
  }
  if (symbol == "square") {
    if (n_val > 1) {
      for (i in 2:n_val) {
        x[i] <- x[1] + (symb_sizes$x[1] - symb_sizes$x[i])
      }
    }
    symbols(
      x = x,
      y = y,
      squares = symb_sizes$inches,
      bg = pal,
      fg = border,
      lwd = lwd,
      add = TRUE,
      inches = inches * 2,
    )
    # display lines
    segments(
      x0 = x[1] + symb_sizes$x[1],
      x1 = x[1] + symb_sizes$x[1] + x_spacing,
      y0 = y + symb_sizes$y,
      y1 = y + symb_sizes$y,
      col = border
    )
    # display labels
    text(
      x = x + x_spacing + symb_sizes$x + x_spacing,
      y = y + symb_sizes$y,
      labels = valleg, cex = val_cex, adj = c(0, 0.5), col = fg
    )
  }


  return(invisible(NULL))
}
