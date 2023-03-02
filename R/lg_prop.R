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
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param border color of the symbols borders
#' @param size size of the legend; 2 means two times bigger
#' @param lwd width of the symbols borders
#' @param col color of the symbols (for "prop") or color of the lines (for
#' "prop_line" and "grad_line")
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param mar plot margins
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param self_adjust if TRUE values are self-adjusted to keep min, max and
#' intermediate rounded values
#' @param box_cex do nothing
#' @param adj adj
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
                    inches = .3,
                    symbol = "circle",
                    border = "#333333",
                    lwd = .7,
                    title = "Legend Title",
                    title_cex = .8 * size,
                    val_cex = .6 * size,
                    val_rnd = 0,
                    frame = FALSE,
                    bg = "#f7f7f7",
                    fg = "#333333",
                    size = 1,
                    self_adjust = FALSE,
                    return_bbox = FALSE,
                    mar = par("mar"),
                    box_cex,
                    adj = c(0,0)) {
  insetf <- strwidth("MM", units = "user", cex = 1)
  inset <- insetf * size

  val <- unique(val)
  if (length(val) == 1) {
    self_adjust <- FALSE
  }

  if (self_adjust == TRUE) {
    val <- self_adjust_v(val, inches, val_cex, mar = mar)
  }
  val <- sort(val, decreasing = TRUE)

  valleg <- get_val_rnd(val = val, val_rnd = val_rnd)
  xy_leg <- NULL

  while (TRUE) {
    if (length(pos) == 2 && is.numeric(pos)) {
      xy_leg <- pos + (c(inset,-inset)) / 4
    }
    xy_title <- get_xy_title(
      x = xy_leg[1],
      y = xy_leg[2],
      title = title,
      title_cex = title_cex
    )
    xy_symbols <- get_xy_s(
      x = xy_title$x,
      y = xy_title$y - inset / 2,
      val = val,
      inches = inches,
      symbol = symbol,
      mar = mar
    )
    xy_lines <- get_xy_lines(
      x = xy_symbols$x[1],
      y = xy_symbols$y,
      sizesi = xy_symbols$s,
      inset = inset / 4
    )
    xy_lab <- get_xy_lab_s(
      x = xy_lines$x1 + inset / 4,
      y = xy_symbols$y + xy_symbols$s,
      val = valleg,
      val_cex = val_cex
    )

    xy_rect <- get_xy_rect_s(
      xy_title = xy_title,
      xy_symbols = xy_symbols,
      xy_lines = xy_lines,
      xy_lab = xy_lab,
      inset = inset
    )

    if (!is.null(xy_leg)) {
      break
    }
    xy_leg <- get_pos_leg(
      pos = pos,
      xy_rect = unlist(xy_rect),
      adj = adj,
      xy_title = xy_title,
      frame = frame
    )
  }


  if (return_bbox) {
    return(invisible(
      list(
        xleft = xy_rect[[1]] - insetf / 4,
        ybottom = xy_rect[[2]] - insetf / 4,
        xright = xy_rect[[3]] + insetf / 4,
        ytop = xy_rect[[4]] + insetf / 4
      )
    ))
  }

  # Display
  if (frame) {
    rect(
      xleft = xy_rect[[1]] - insetf / 4,
      ybottom = xy_rect[[2]] - insetf / 4,
      xright = xy_rect[[3]] + insetf / 4,
      ytop = xy_rect[[4]] + insetf / 4,
      col = bg,
      border = fg,
      lwd = .7
    )
  }
  text(
    xy_title$x,
    y = xy_title$y,
    labels = title,
    cex = title_cex,
    adj = c(0, 0),
    col = fg
  )
  dots <- data.frame(xy_symbols$x, xy_symbols$y)

  plot_symbols(
    symbol = symbol,
    dots = dots,
    sizes = xy_symbols$s,
    mycols = col,
    border = border,
    lwd = lwd,
    inches = inches
  )
  segments(
    x0 = xy_lines$x0,
    x1 = xy_lines$x1,
    y0 = xy_lines$y0,
    y1 = xy_lines$y1,
    col = border
  )
  text(
    xy_lab$x,
    y = xy_lab$y,
    labels = rev(valleg),
    cex = val_cex,
    adj = c(0, 0.5),
    col = fg
  )

  return(invisible(NULL))
}
