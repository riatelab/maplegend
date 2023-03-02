## Return well formated rounded values.
get_val_rnd <- function(val, val_rnd) {
  if (is.numeric(val)) {
    val <- round(val, val_rnd)
    if (val_rnd <= 0) {
      val_rnd <- 0
    }
    val <- format(x = val, scientific = FALSE, nsmall = val_rnd)
    val <- trimws(val)
  }
  val
}

#' Title
#'
#' @param pal pal
#' @param nbreaks nbreaks
#' @param alpha alpha
#' @noRd
#' @importFrom grDevices hcl.pals hcl.colors
get_pal <- function(pal, nbreaks, alpha = 1) {
  if (length(pal) == 1) {
    if (pal %in% hcl.pals()) {
      cols <- hcl.colors(n = nbreaks, palette = pal, alpha = alpha, rev = TRUE)
    } else {
      cols <- rep(pal, nbreaks)
    }
  } else {
    cols <- pal[1:nbreaks]
  }
  return(cols)
}











# get position and size of the title
get_xy_title <- function(x = NULL, y, title, title_cex) {
  h <- strheight(title, units = "user", cex = title_cex, font = 1)
  w <- strwidth(title, units = "user", cex = title_cex, font = 1)
  if (is.null(x)) {
    x <- 0
    y <- 0 - h
  } else {
    y <- y - h
  }
  return(list(x = x, y = y, h = h, w = w))
}



interleg <- function(txt = c("legend", "Legend")) {
  if (interactive()) {
    message(paste0("Click on the map to choose the ", txt[1], " position."))
    x <- unlist(locator(1))
    message(paste0(txt[2], " coordinates:\nc(", x[[1]], ", ", x[[2]], ")"))
    return(x)
  } else {
    stop('You cannot use "interactive" in a non-interactive R session.',
         call. = FALSE
    )
  }
}







# get position of the NA box
get_xy_nabox <- function(x, y, w, h) {
  xleft <- x
  xright <- x + w
  ytop <- y
  ybottom <- y - h
  return(list(
    xleft = unname(xleft),
    ybottom = unname(ybottom),
    xright = unname(xright),
    ytop = unname(ytop),
    h = h,
    w = w
  ))
}



# get na box label pos
get_xy_nabox_lab <- function(x, y, h, no_data_txt, val_cex) {
  y <- y - h / 2
  w <- max(strwidth(no_data_txt, units = "user", cex = val_cex, font = 1))
  return(list(x = x, y = y, w = w))
}



# get frame coordinates
get_xy_rect <- function(xy_title, xy_box, xy_nabox,
                        xy_box_lab, xy_nabox_lab, no_data,
                        inset, w, cho = FALSE) {
  if (cho && !no_data) {
    xy_box$h <- xy_box$h + (xy_box_lab$h / 2)
  }
  xy_leg <- list(
    xleft = xy_title$x,
    ybottom =
      xy_title$y - inset / 2 -
      xy_box$h -
      (xy_nabox$h + inset / 2) * no_data,
    xright = xy_title$x +
      max(
        xy_title$w,
        w + inset / 4 + xy_box_lab$w,
        (w + inset / 4 + xy_nabox_lab$w) * no_data
      ),
    ytop = xy_title$y + xy_title$h
  )
  xy_leg
}





#' @name get_size
#' @title get_size
#' @description get a vector of radii
#' @param inches inches
#' @param var var
#' @param fixmax fixmax
#' @param symbol symbols
#' @return a vector of radii
#' @noRd
get_size <- function(var, inches, val_max, symbol) {
  switch(symbol,
         circle = {
           smax <- inches * inches * pi
           size <- sqrt((var * smax / val_max) / pi)
         },
         square = {
           smax <- inches * inches
           size <- sqrt(var * smax / val_max)
         }
  )
  return(size)
}












# get the position of the legeng
get_pos_leg <- function(pos, xy_rect, adj, xy_title, frame = FALSE) {
  pu <- par("usr")
  inset2 <- strwidth("M", units = "user", cex = 1) / 2
  if (frame) {
    pu <- pu + c(inset2, -inset2, inset2, -inset2)
  }

  extra <- inset2 * 2 * adj

  xy <- switch(
    pos,
    bottomleft = c(
      pu[1] + inset2,
      pu[3] + xy_rect[4] - xy_rect[2] + inset2
    ),
    topleft = c(
      pu[1] + inset2,
      pu[4] - inset2
    ),
    left = c(
      pu[1] + inset2,
      pu[3] + (pu[4] - pu[3]) / 2 + (xy_rect[4] - xy_rect[2]) / 2 - inset2
    ),
    top = c(
      pu[1] + (pu[2] - pu[1]) / 2 - (xy_rect[3] - xy_rect[1]) / 2 + inset2,
      pu[4] - inset2
    ),
    bottom = c(
      pu[1] + (pu[2] - pu[1]) / 2 - (xy_rect[3] - xy_rect[1]) / 2 + inset2,
      pu[3] + xy_rect[4] - xy_rect[2] + inset2
    ),
    bottomright = c(
      pu[2] - xy_rect[3] - xy_rect[1] - inset2,
      pu[3] + xy_rect[4] - xy_rect[2] + inset2
    ),
    right = c(
      pu[2] - xy_rect[3] - xy_rect[1] - inset2,
      pu[3] + (pu[4] - pu[3]) / 2 + (xy_rect[4] - xy_rect[2]) / 2 - inset2
    ),
    topright = c(
      pu[2] - xy_rect[3] - xy_rect[1] - inset2,
      pu[4] - inset2
    ),
    interactive = interleg()
  )

  xy <- xy + extra

  return(unname(xy))
}
