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
#' @param alpha opacity, in the range [0,1]
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param mar plot margins
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
                     frame = FALSE,
                     frame_border = fg,
                     bg = "#f7f7f7",
                     fg = "#333333",
                     size = 1,
                     self_adjust = FALSE,
                     return_bbox = FALSE,
                     mar = par("mar"),
                     box_cex,
                     adj = c(0, 0)) {
  insetf <- xinch(par("csi"))
  inset <- strwidth("MM", units = "user", cex = 1) * size

  if (!is.null(alpha)) {
    col <- get_hex_pal(col, alpha)
  }

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
      xy_leg <- pos + (c(inset, -inset)) / 4
    }
    xy_title <- get_xy_title(
      x = xy_leg[1],
      y = xy_leg[2],
      title = title,
      title_cex = title_cex
    )

    xy_symbols <- get_xy_s(
      x = xy_title$x,
      y = xy_title$y - inset / 3,
      val = val,
      inches = inches,
      symbol = symbol,
      mar = mar
    )



    xy_lines <- get_xy_lines(
      x = xy_symbols$x[1],
      y = xy_symbols$y,
      sizesi = xy_symbols$s,
      inset = inset / 4,
      symbol = symbol
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
      border = frame_border,
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

  vcols <- rep(NA, nrow(dots))
  vcols[1] <- col

  plot_symbols(
    symbol = symbol,
    dots = dots,
    sizes = xy_symbols$s,
    mycols = vcols,
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


myinch <- function(x, mar) {
  op <- par(mar = mar, no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  x * diff(par("usr")[3:4]) / par("pin")[2L]
}


# get prop symbols size and dim  from topleft corner
get_xy_s <- function(x, y, val, inches, symbol, mar) {
  sizes <- get_size(
    var = val,
    inches = inches,
    val_max = max(val),
    symbol = symbol
  )
  sizesi <- myinch(sizes, mar)
  x <- rep(x + sizesi[1], length(val))
  y <- y - sizesi[1] * 2 + sizesi
  h <- sizesi[1] * 2
  w <- h

  if (symbol == "square") {
    n <- length(val)
    if (n > 1) {
      for (i in 2:n) {
        x[i] <- x[1] + (sizesi[1] - sizesi[i])
      }
    }
  }

  return(list(x = x, y = y, s = sizesi, h = h, w = w))
}

# lines from top of symbols to labels
get_xy_lines <- function(x, y, sizesi, inset, symbol) {
  x0 <- rep(x, length(sizesi))
  x1 <- rep(x + sizesi[1] + inset, length(sizesi))
  w <- x1[1] - x0[1] + inset
  if (symbol == "square") {
    x0 <- rep(x + sizesi[1], length(sizesi))
  }
  y0 <- y1 <- y + sizesi
  return(list(x0 = x0, x1 = x1, y0 = y0, y1 = y1, w = w))
}

# get labels position
get_xy_lab_s <- function(x, y, val, val_cex) {
  w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  y <- rev(y)
  return(list(x = x, y = y, w = w))
}

# box around the legend
get_xy_rect_s <- function(xy_title, xy_symbols, xy_lines, xy_lab, inset) {
  xy_leg <- list(
    xleft = xy_title$x,
    ybottom = xy_title$y - inset / 2 - xy_symbols$h,
    xright =
      xy_title$x +
        max(
          xy_title$w,
          xy_symbols$h / 2 +
            xy_lines$w +
            xy_lab$w
        ),
    ytop = xy_title$y + xy_title$h
  )
  xy_leg
}



self_adjust_v <- function(var, inches, val_cex, mar) {
  # get min & max
  val <- c(min(var), max(var))
  # factors
  b <- c(5, 2.5, 1)
  # min val
  min_s <- min(val)
  # max val
  max_s <- max(val)
  # get candidat values for the legend
  ndmax <- floor(log10(max_s))
  if (min_s < 1) {
    ndmin <- nchar(as.character(signif(min_s, digits = 0))) - 2
  } else {
    ndmin <- 1
  }
  i <- c(-ndmin:ndmax)
  v <- vector("numeric", 0)
  for (base in b) {
    v <- c(v, base * 10^i)
  }

  v <- c(max_s, min_s, v)
  v <- v[v >= min_s]
  v <- v[v <= max_s]
  v <- sort(unique(v))

  # circle sizes in map units for candidate values
  si <- myinch(get_size(
    var = v,
    inches = inches,
    val_max = max(val),
    symbol = "circle"
  ), mar = mar)
  # texte size labels in map units
  h <- max(strheight(val, units = "user", cex = val_cex, font = 1)) * 1.2

  # number of candidate values
  i <- length(si)

  # vector of displayed values
  a <- vector("logical", i)

  # The last one (max) is always displayed
  a[i] <- TRUE

  # go to next one
  i <- i - 1
  while (TRUE) {
    maxv <- si[length(si)] * 2
    # test space between two circles
    if (maxv - si[i] * 2 <= h) {
      # the space is too small
      a[i] <- FALSE
      # go to next value
      si <- si[-(length(si) - 1)]
    } else {
      # the space is not too small
      si <- si[-length(si)]
      # display ok
      a[i] <- TRUE
    }
    # increment
    i <- i - 1
    # last value
    if (i == 0) break
  }

  # If only one value is selected (max) select also the lower
  if (sum(a) <= 1) {
    a[1] <- TRUE
  }

  # if min value not selected, remove min selected value and replace
  # with min value
  if (a[1] == FALSE) {
    a[which(a)[1]] <- FALSE
    a[1] <- TRUE
  }

  return(v[a])
}






# Plot symbols
plot_symbols <- function(symbol, dots, sizes, mycols, border, lwd, inches) {
  switch(symbol,
    circle = {
      symbols(
        x = dots[, 1],
        y = dots[, 2],
        circles = sizes,
        bg = mycols,
        fg = border,
        lwd = lwd,
        add = TRUE,
        inches = inches,
        asp = 1
      )
    },
    square = {
      symbols(
        x = dots[, 1],
        y = dots[, 2],
        squares = sizes,
        bg = mycols,
        fg = border,
        lwd = lwd,
        add = TRUE,
        inches = inches * 2,
        asp = 1
      )
    }
  )
}
