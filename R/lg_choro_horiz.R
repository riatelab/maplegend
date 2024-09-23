#' @title Plot a legend for a choropleth map
#' @description This function plots a legend for a choropleth map.
#'
#' @param pal a set of colors.
#' @param col_na color for missing values
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y))
#' @param val break labels
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param box_border color of the boxes' borders
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param box_cex width and height cex of boxes
#' @param mar plot margins
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
leg_choro_h <- function(pos = "left",
                        val,
                        pal = "Inferno",
                        title = "Legend Title",
                        title_cex = .8 * size,
                        val_cex = .6 * size,
                        val_rnd = 0,
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
  w <- inset * 2
  # box height
  h <- inset / 3
  if (length(box_cex) == 2) {
    w <- w * box_cex[1]
    h <- h * box_cex[2]
  }

  # get well rounded and ordered values for the legend
  val <- get_val_rnd(val = val, val_rnd = val_rnd)

  # number of boxes
  n <- length(val) - 1

  # box colors
  pal <- get_pal(pal, n)

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
      x = xy_title$x + strwidth(val[1], units = "user", cex = val_cex, font = 1) / 2,
      y = xy_title$y - inset / 3,
      n = n,
      w = w,
      h = h,
      inset = 0,
      type = "c_h"
    )

    xy_nabox <- get_xy_nabox(
      x = xy_box$xright[n] + inset / 2,
      y = xy_box$ytop[1],
      w = w,
      h = h
    )


    xy_box_lab <- get_xy_box_lab(
      x = xy_box$xleft[1],
      y = xy_box$ybottom[1] -
        strheight(val[1], units = "user", cex = val_cex, font = 1) -
        inset / 6,
      w = w,
      val = val,
      val_cex = val_cex,
      type = "c_h"
    )




    # get na box label pos
    xy_nabox_lab <- list(
      x = xy_nabox$xleft + (xy_nabox$xright - xy_nabox$xleft) / 2,
      y = xy_nabox$ybottom -
        strheight(val[1], units = "user", cex = val_cex, font = 1) -
        inset / 6,
      h = strheight(no_data_txt, units = "user", cex = val_cex, font = 1),
      w = max(c(
        xy_nabox$xleft - xy_nabox$xright,
        strwidth(no_data_txt, units = "user", cex = val_cex, font = 1) * no_data
      ))
    )

    if (no_data) {
      xrightx <- max(
        c(
          xy_nabox$xright,
          xy_nabox_lab$x +
            strwidth(no_data_txt, units = "user", cex = val_cex, font = 1) / 2
        )
      )
    } else {
      xrightx <- xy_box$xright[n] +
        strwidth(val[n + 1], units = "user", cex = val_cex, font = 1) / 2
    }

    xy_rect <- list(
      xleft = xy_title$x,
      ybottom = xy_box_lab$y[1] - insetf / 8,
      xright = xrightx,
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
  rect(
    xy_box[[1]],
    xy_box[[2]],
    xy_box[[3]],
    xy_box[[4]],
    col = pal,
    border = box_border,
    lwd = .7
  )
  # labels
  text(
    xy_box_lab$x,
    y = xy_box_lab$y,
    labels = val,
    cex = val_cex,
    adj = c(0.5, 0),
    col = fg
  )
  # no data
  if (no_data) {
    # NA box
    rect(
      xy_nabox[[1]],
      xy_nabox[[2]],
      xy_nabox[[3]],
      xy_nabox[[4]],
      col = col_na,
      border = box_border,
      lwd = .7
    )
    # NA text
    text(
      x = xy_nabox_lab$x,
      y = xy_nabox_lab$y,
      labels = no_data_txt,
      cex = val_cex,
      adj = c(0.5, 0),
      col = fg
    )
  }

  return(invisible(NULL))
}
