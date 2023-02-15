#' @title Compose a map legend
#' @description Compose a map legend with several elements.
#' The "type" argument defines the
#' legend type:
#' * **prop** for proportional symbols,
#' * **choro** for choropleth maps,
#' * **cont** for continuous maps (e.g. raster),
#' * **typo** for typology maps,
#' * **symb** for symbols maps,
#' * **prop_line** for proportional lines maps,
#' * **grad_line** for graduated lines maps.
#'
#' Please note that some arguments are available for all types of legend and
#' some others are only relevant for specific legend types (see Details).
#'
#'
#' @md
#' @param leg legend object
#' @param type type of legend:
#' * **prop** for proportional symbols,
#' * **choro** for choropleth maps,
#' * **cont** for continuous maps,
#' * **typo** for typology maps,
#' * **symb** for symbols maps,
#' * **prop_line** for proportional lines maps,
#' * **grad_line** for graduated lines maps.
#' @param val
#' vector of value(s) (for "prop" and "prop_line", at least c(min, max) for "cont"),
#' vector of categories (for "symb" and "typo"),
#' break labels (for "choro" and "grad_line").
#' @param pal a color palette name or a vector of colors
#' @param inches size of the largest symbol (radius for circles, half width
#' for squares) in inches
#' @param border symbol border color(s)
#' @param symbol type of symbols, 'circle' or 'square'
#' @param self_adjust if TRUE values are self-adjusted to keep min, max and
#' intermediate rounded values
#' @param title title of the legend
#' @param val_rnd number of decimal places of the values in
#' the legend
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values
#' @param box_border border color of legend boxes
#' @param box_cex width and height size expansion of boxes,
#' (or offset between circles for "prop" legends with horiz = TRUE)
#' @param col color of the symbols (for "prop") or color of the lines (for
#' "prop_line" and "grad_line")
#' @param lwd width(s) of the symbols borders (for "prop" and "symb"),
#' width of the largest line (for "prop_line"), vector of line width
#' (for "grad_line")
#' @param cex size(s) of the symbols
#' @param pch type(s) of the symbols (0:25)
#' @param col_na color for missing values
#' @param cex_na size of the symbols for missing values
#' @param pch_na type of the symbols for missing values
#' @param horiz if TRUE plot an horizontal legend
#' @return No return value, a legend is displayed.
#' @export
#' @details
#' Some arguments are available for all types of legend: `val`, `pos`, `title`,
#' `title_cex`, `val_cex`, `frame`, `bg`, `fg`, `size`, `return_bbox` and `mar`).
#'
#'
#' Relevant arguments for each specific legend types:
#' * `leg(type = "prop", val, inches, symbol, col, lwd, border, val_rnd, self_adjust, horiz)`
#' * `leg(type = "choro", val, pal, val_rnd, col_na, no_data, no_data_txt, box_border, horiz)`
#' * `leg(type = "cont", val, pal, val_rnd, col_na, no_data, no_data_txt, box_border, horiz)`
#' * `leg(type = "typo", val, pal, col_na, no_data, no_data_txt, box_border)`
#' * `leg(type = "symb", val, pal, pch, cex, lwd, pch_na, cex_na, col_na, no_data, no_data_txt)`
#' * `leg(type = "prop_line", val, col, lwd, val_rnd)`
#' * `leg(type = "grad_line", val, col, lwd, val_rnd)`
#'
#' Legend positions ending with a number ("topleft1", "topleft2"...) are placed using a vertical offset.
#' This offset has the size of one (or two) character height and allows to plot a text below or on top of the legend.
#'
#'
#' @examples
#' # minimal example
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' box()
#' leg_comp(type = "prop", val = c(10,50,100)) |>
#' leg_comp(type = "typo", val = c("A", "B", "C")) |>
#' leg_draw()
leg_comp <- function(leg,
                     type,
                     val,
                     pal = "Plasma",
                     col = "tomato4",
                     inches = .3,
                     symbol = "circle",
                     self_adjust = FALSE,
                     lwd = 0.7,
                     border = "#333333",
                     pch = 1:length(val),
                     cex = rep(1, length(val)),
                     title = "Legend Title",
                     val_rnd = 0,
                     col_na = "white",
                     cex_na = 1,
                     pch_na = 4,
                     no_data = FALSE,
                     no_data_txt = "No Data",
                     box_border = "333333",
                     box_cex = c(1, 1),
                     horiz = FALSE){
  res <- as.list(match.call())
  res <- res[-1]
  res <- lapply(res, eval)
  res <- clean_input(res, type = type)
  if (missing(leg)) {
    leg <- list()
  }
  leg$layers[[length(leg$layers) + 1]] <- res
  return(leg)
}



clean_input <- function(res, type) {
  res <- res[unlist(lapply(
    X = res,
    FUN = function(x) {
      !is.name(x)
    }
  ))]
  res$leg <- NULL
  res$type <- type
  res
}


#' @title Draw a map legend
#' @description Draw a map legend with several elements.
#' @param x legend
#' @param pos pos
#' @param size size
#' @param bg bg
#' @param fg fg
#' @param frame frame
#' @param mar mar
#'
#' @return
#' @export
#'
#' @examples
#' plot(1:10)
leg_draw <- function(x,
                     pos = "bottomright",
                     size = 1,
                     bg = 'white',
                     fg = 'black',
                     frame = TRUE,
                     mar = par("mar")) {
  dimleg <- list()
  insetf <- strwidth("MM", units = "user", cex = 1)

  for (i in 1:length(x$layers)) {
    x$layers[[i]]$pos <- pos
    x$layers[[i]]$return_bbox <- TRUE
    x$layers[[i]]$mar <- mar
    x$layers[[i]]$size <- size
    x$layers[[i]]$frame <- frame
    dimleg[[i]] <- do.call(leg, x$layers[[i]])
  }

  if (pos == "bottomright") {
    xleg <- min(unlist(lapply(dimleg, function(x) {
      x$xleft
    })))
    xleg <- rep(xleg, length(dimleg))
    sizes <- unlist(lapply(dimleg, function(x) {
      x$ytop - x$ybottom
    }))
    ybottominit <-
      min(unlist(lapply(dimleg, function(x) {
        x$ybottom
      })))
    yleg  <- ybottominit + rev(cumsum(rev(sizes)))

    frame_c <- c(
      xmin = xleg[1],
      xmax = min(unlist(lapply(dimleg, function(x) {
        x$xright
      }))),
      ymin = min(unlist(lapply(dimleg, function(x) {
        x$ybottom
      }))),
      ymax = yleg[1]
    )
  }

  if (frame) {
    rect(
      xleft = frame_c[1],
      ybottom = frame_c[3],
      xright = frame_c[2],
      ytop = frame_c[4],
      col = bg,
      border = fg,
      xpd = TRUE
    )
  }

  s = 0
  for (i in 1:length(x$layers)) {
    x$layers[[i]]$mar <- mar
    x$layers[[i]]$pos <- c(xleg[i] + s , yleg[i] - s)
    x$layers[[i]]$return_bbox <- FALSE
    x$layers[[i]]$frame <- FALSE
    x$layers[[i]]$size <- size
    x$layers[[i]]$bg <- bg
    x$layers[[i]]$fg <- fg
    do.call(leg, x$layers[[i]])
  }

  return(invisible(NULL))
}
