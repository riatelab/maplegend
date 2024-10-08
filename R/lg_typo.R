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
#' leg_typo(val = c("type A", "type B"), pal = c("navy", "tomato"))
leg_typo <- function(pos = "topright",
                     val,
                     pal = "Inferno",
                     alpha = 1,
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
                     mar = par("mar"),
                     adj = c(0, 0)) {
  insetf <- xinch(par("csi"))
  inset <- strwidth("MM", units = "user", cex = 1) * size

  # box size mgmt
  # box width
  w <- inset
  # box height
  h <- inset / 1.5
  if (length(box_cex) == 2) {
    w <- w * box_cex[1]
    h <- h * box_cex[2]
  }

  # number of boxes
  n <- length(val)
  # box colors
  pal <- get_pal(pal, n, alpha = alpha)

  # initiate xy leg position
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
      w = w,
      h = h,
      inset = inset / 4,
      type = "t"
    )
    xy_nabox <- get_xy_nabox(
      x = xy_title$x,
      y = xy_box$ybottom[n] - inset / 4,
      w = w,
      h = h
    )
    xy_box_lab <- get_xy_box_lab(
      x = xy_box$xright[n] + inset / 4,
      y = xy_box$ytop[1],
      h = h,
      val = val,
      val_cex = val_cex,
      inset = inset / 4,
      type = "t"
    )
    xy_nabox_lab <- get_xy_nabox_lab(
      x = xy_nabox$xright + inset / 4,
      y = xy_nabox$ytop,
      h = h,
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
      w = w,
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
  rect(
    xy_box[[1]],
    xy_box[[2]],
    xy_box[[3]],
    xy_box[[4]],
    col = pal,
    border = box_border,
    lwd = .7
  )
  text(
    xy_box_lab$x,
    y = xy_box_lab$y,
    labels = val,
    cex = val_cex,
    adj = c(0, 0.5),
    col = fg
  )
  if (no_data) {
    rect(
      xy_nabox[[1]],
      xy_nabox[[2]],
      xy_nabox[[3]],
      xy_nabox[[4]],
      col = col_na,
      border = box_border,
      lwd = .7
    )
    text(
      xy_nabox_lab$x,
      y = xy_nabox_lab$y,
      labels = no_data_txt,
      cex = val_cex,
      adj = c(0, 0.5),
      col = fg
    )
  }


  return(invisible(NULL))
}
