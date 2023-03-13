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
#' @param box_cex width and height cex of boxes
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param adj adj
#' @param self_adjust if TRUE values are self-adjusted to keep min, max and
#' intermediate rounded values
#' @keywords internal
#' @noRd
#' @return No return value, a legend is displayed.
#' @import graphics
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' leg_prop(val = c(1, 20, 100), col = "red", inches = .3)
leg_prop_h <- function(pos = "left",
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
                     box_cex = 1,
                     self_adjust = FALSE,
                     return_bbox = FALSE,
                     mar = par("mar"),
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

    n <- length(val)
    xx <- xy_symbols$x
    ss <- xy_symbols$s
    xx[1] <- xx[1]
    if(n>1){
      for(i in 2:n){
        xx[i] <- xx[i-1] + sum(ss[(i-1):i])
        xx[i] = xx[i] + inset / 8 * box_cex
      }

    }
    xy_symbols$x <- xx


    xy_lab <- list(
      x = xy_symbols$x,
      y = xy_symbols$y -
        xy_symbols$s -
        strheight(val[1], units = "user", cex = val_cex, font = 1) - inset / 8,
      w = strheight(val, units = "user", cex = val_cex, font = 1)
    )

    xy_rect <- list(
      xleft = xy_title$x,
      ybottom = xy_lab$y[1],
      xright =
        max(
          xy_title$x + xy_title$w,
          xy_symbols$x[n],
          xy_lab$x[n]+ xy_lab$w
        ),
      ytop = xy_title$y + xy_title$h
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

  text(
    xy_lab$x,
    y = xy_lab$y,
    labels = valleg,
    cex = val_cex,
    adj = c(0.5, 0),
    col = fg
  )

  return(invisible(NULL))
}
