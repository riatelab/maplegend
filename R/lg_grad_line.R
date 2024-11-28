#' @title Plot a legend for a graduated lines map
#' @description This function plots a legend for graduated lines maps.
#'
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y))
#' @param lwd lines widths
#' @param val break labels
#' @param col lines color
#' @param alpha opacity, in the range \[0,1\]
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
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
#' leg_grad_line(lwd = c(0.2, 2, 4, 5, 10), val = c(1, 2, 3, 4, 10.2, 15.2))
leg_grad_line <- function(pos = "topleft",
                          val,
                          col = "tomato4",
                          alpha = NULL,
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
                          box_cex = c(1, 1),
                          return_bbox = FALSE,
                          mar = par("mar"),
                          adj = c(0, 0)) {
  insetf <- xinch(par("csi"))
  inset <- strwidth("MM", units = "user", cex = 1) * size

  if (!is.null(alpha)) {
    col <- get_hex_pal(col, alpha)
  }

  # box size mgmt
  # box width
  w <- inset
  # box height
  h <- inset / 1.5
  if (length(box_cex) == 2) {
    w <- w * box_cex[1]
    h <- h * box_cex[2]
  }

  val <- rev(get_val_rnd(val = val, val_rnd = val_rnd))

  lwd <- rev(lwd)
  n <- length(val) - 1
  pal <- rev(col)

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
      y = xy_title$y - inset / 2,
      n = n,
      w = w,
      h = h,
      inset = 0,
      type = "c"
    )

    xy_box_lab <- get_xy_box_lab(
      x = xy_box$xright[n] + inset / 4,
      y = xy_box$ytop[1],
      h = h,
      val = val,
      val_cex = val_cex,
      type = "c"
    )

    xy_box$h <- xy_box$h + inset / 4
    xy_rect <- get_xy_rect_l(
      xy_title = xy_title,
      xy_box = xy_box,
      xy_box_lab = xy_box_lab,
      inset = inset,
      w = w
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

  # title
  text(
    xy_title$x,
    y = xy_title$y,
    labels = title,
    cex = title_cex,
    adj = c(0, 0),
    col = fg
  )
  # boxes
  segments(
    x0 = xy_box[[1]],
    y0 = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2,
    x1 = xy_box[[3]],
    y1 = xy_box[[2]] + (xy_box[[4]] - xy_box[[2]]) / 2,
    col = pal,
    lwd = lwd,
    lend = 1
  )
  # labels
  text(
    xy_box_lab$x,
    y = xy_box_lab$y,
    labels = val,
    cex = val_cex,
    adj = c(0, 0.5),
    col = fg
  )

  return(invisible(NULL))
}
