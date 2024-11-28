#' Plot a legend for a symbols map
#' @description This function can plot a legend for a symbols maps.
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
#' @param border type = "prop": color of the symbols borders
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param cex cex of the symbols
#' @param pch pch of the symbols (0:25)
#' @param cex_na cex of the symbols for missing values
#' @param pch_na pch of the symbols for missing values
#' @param lwd width of the symbols borders,
#' @param box_cex width and height cex of boxes
#' @param mar plot margins
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
#' leg_symb(
#'   val = c("Type C", "Type D"), pal = c("cyan", "plum"),
#'   pch = c(21, 23), cex = c(1, 2)
#' )
leg_symb <- function(pos = "left",
                     val,
                     pal = "Inferno",
                     alpha = NULL,
                     pch = seq_along(val),
                     cex = rep(1, length(val)),
                     border = "#333333",
                     lwd = .7,
                     title = "Legend title",
                     title_cex = .8 * size,
                     val_cex = .6 * size,
                     cex_na = 1,
                     pch_na = 4,
                     col_na = "white",
                     no_data = FALSE,
                     no_data_txt = "No Data",
                     frame = FALSE,
                     frame_border = fg,
                     bg = "#f7f7f7",
                     fg = "#333333",
                     size = 1,
                     box_cex = c(1, 1),
                     return_bbox = FALSE,
                     mar = par("mar"),
                     adj = c(0, 0)) {
  insetf <- xinch(par("csi"))
  inset <- strwidth("MM", units = "user", cex = 1) * size

  # box size mgmt
  n <- length(val)
  if (length(cex) != n) {
    cex <- rep(cex[1], n)
  }
  if (length(lwd) != n) {
    lwd <- rep(lwd[1], n)
  }

  s_cex <- cex
  for (i in seq_along(cex)) {
    s_cex[i] <- strheight("M", units = "user", cex = s_cex[i]) * .7
  }
  w_cex <- s_cex
  h_cex <- s_cex

  if (length(box_cex) == 2) {
    w_cex <- w_cex * box_cex[1]
    h_cex <- h_cex * box_cex[2]
  }
  hh <- strheight(val, units = "user", cex = val_cex)
  h_cex[h_cex < hh] <- hh[h_cex < hh]

  s_cex_na <- strheight("M", units = "user", cex = cex_na) * .7
  w_cex_na <- s_cex_na
  h_cex_na <- s_cex_na

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


    xy_box <- get_xy_box(
      x = xy_title$x,
      y = xy_title$y - inset / 3,
      n = n,
      w = w_cex,
      h = h_cex,
      inset = inset / 3,
      type = "s"
    )
    xy_nabox <- get_xy_nabox(
      x = xy_title$x,
      y = xy_box$ybottom[n] - inset / 2,
      w = w_cex_na,
      h = h_cex_na
    )

    xy_box_lab <- get_xy_box_lab(
      x = xy_title$x + max(c(w_cex, w_cex_na)) + inset / 4,
      y = xy_title$y - inset / 3,
      h = h_cex,
      val = val,
      val_cex = val_cex,
      inset = inset / 3,
      type = "s"
    )

    xy_nabox_lab <- get_xy_nabox_lab(
      x = xy_title$x + max(c(w_cex, w_cex_na)) + inset / 4,
      y = xy_nabox$ytop,
      h = h_cex_na,
      no_data_txt = no_data_txt,
      val_cex = val_cex
    )


    xy_rect <- get_xy_rect(
      xy_title = xy_title,
      xy_box = xy_box,
      xy_nabox = xy_nabox,
      xy_box_lab = xy_box_lab,
      xy_nabox_lab = xy_nabox_lab,
      no_data = no_data,
      inset = inset,
      w = max(c(w_cex, w_cex_na)),
      cho = FALSE
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

  # centepoints
  if (no_data) {
    lv <- max(xy_box[[3]], xy_nabox[[3]])
    lt <- max(c(w_cex, w_cex_na))
  } else {
    lv <- max(xy_box[[3]])
    lt <- max(c(w_cex))
  }

  pal <- get_pal(pal, n, alpha = alpha)
  mycolspt <- pal

  if (length(pch) == 1) {
    pch <- rep(pch, n)
  }

  if (any(pch %in% 21:25)) {
    mycolspt[pch %in% 21:25] <- border
  }
  mycolsptbg <- pal

  for (i in seq_along(xy_box[[1]])) {
    points(
      xy_box[[1]][i] + (lv - xy_box[[1]][i]) / 2,
      xy_box[[2]][i] + (xy_box[[4]][i] - xy_box[[2]][i]) / 2,
      col = mycolspt[i],
      pch = pch[[i]],
      cex = cex[i],
      bg = mycolsptbg[i],
      lwd = lwd[i]
    )
  }
  text(
    xy_box[[1]] + lt + inset / 4,
    y = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2,
    labels = val,
    cex = val_cex,
    adj = c(0, 0.5),
    col = fg
  )
  if (no_data) {
    col_nafg <- col_na
    col_nafg[pch_na %in% 21:25] <- border
    col_nabg <- col_na
    points(
      xy_nabox[[1]] + (lv - xy_nabox[[1]]) / 2,
      xy_nabox[[2]] + (xy_nabox[[4]] - xy_nabox[[2]]) / 2,
      col = col_nafg,
      pch = pch_na,
      cex = cex_na,
      bg = col_nabg,
      lwd = lwd
    )
    text(
      xy_nabox[[1]] + lt + inset / 4,
      y = xy_nabox[[2]] + (xy_nabox[[4]] - xy_nabox[[2]]) / 2,
      labels = no_data_txt,
      cex = val_cex,
      adj = c(0, 0.5),
      col = fg
    )
  }



  return(invisible(NULL))
}
