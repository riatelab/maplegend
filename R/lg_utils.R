## Return well formated rounded numeric values.
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

# get frame coordinates when using lines
get_xy_rect_l <- function(xy_title, xy_box,
                          xy_box_lab,
                          inset, w) {
  xy_leg <- list(
    xleft = xy_title$x,
    ybottom =
      xy_title$y - inset / 2 -
        xy_box$h,
    xright = xy_title$x +
      max(
        xy_title$w,
        w + inset / 4 + xy_box_lab$w
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

  xy <- switch(pos,
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






leg_test_pos <- function(pos) {
  authorized_pos <-
    c(
      "bottomleft",
      "left",
      "topleft",
      "top",
      "bottom",
      "bottomright",
      "right",
      "topright",
      "interactive"
    )

  # stop if the position is not valid
  if (length(pos) == 1) {
    if (!pos %in% authorized_pos) {
      stop("This legend position is not allowed", call. = FALSE)
    }
  }
}

#' @importFrom grDevices dev.list
leg_test_cur_plot <- function() {
  if (is.null(dev.list())) {
    stop("You can only plot legends on an existing plot.", call. = FALSE)
  }
}

leg_test_input <- function(pos) {
  leg_test_pos(pos)
  leg_test_cur_plot()
}



get_xy_box <- function(x, y, n, w, h, inset, type = c("c", "c_h", "t", "s")) {
  if (type %in% c("c", "t")) {
    xleft <- rep(x, n)
    xright <- rep(x + w, n)
    ytop <- rep(NA, n)
    ybottom <- rep(NA, n)
    for (i in 1:n) {
      ytop[i] <- y - (i - 1) * h - (i - 1) * inset
      ybottom[i] <- ytop[i] - h
    }
    h <- ytop[1] - ybottom[n]
    w <- w
  }
  if (type == "s") {
    xleft <- rep(x, n)
    xright <- x + w
    ytop <- rep(NA, n)
    ybottom <- rep(NA, n)
    ytop[1] <- y
    ybottom[1] <- y - h[1]
    if (n >= 2) {
      for (i in 2:n) {
        ytop[i] <- ybottom[i - 1] - inset
        ybottom[i] <- ytop[i] - h[i]
      }
    }
    h <- ytop[1] - ybottom[n]
    w <- max(xright) - x
  }

  if (type == "c_h") {
    ytop <- rep(y, n)
    ybottom <- rep(y - h, n)
    xright <- rep(NA, n)
    xleft <- rep(NA, n)
    for (i in 1:n) {
      xleft[i] <- x + (i - 1) * w
      xright[i] <- xleft[i] + w
    }
    h <- h
    w <- xright[n] - xleft[1]
  }


  return(list(
    xleft = unname(xleft),
    ybottom = unname(ybottom),
    xright = unname(xright),
    ytop = unname(ytop),
    h = h,
    w = w
  ))
}




get_xy_box_lab <- function(x, y, h, w, val, val_cex, inset,
                           type = c("c", "c_h", "t", "s")) {
  n <- length(val)

  if (type %in% c("t", "s", "c")) {
    xc <- rep(x, n)
    yc <- rep(NA, n)

    if (type == "t") {
      for (i in 1:n) {
        yc[i] <- y - (i - 1) * h - h / 2 - (i - 1) * inset
      }
    } else if (type == "s") {
      for (i in 1:n) {
        yc[i] <- y - (i - 1) * h[i] - h[i] / 2 - (i - 1) * inset
      }
    } else if (type == "c") {
      for (i in 1:n) {
        yc[i] <- y - (i - 1) * h
      }
    }
    w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  }
  if (type == "c_h") {
    xc <- rep(NA, n)
    yc <- rep(y, n)
    for (i in 1:n) {
      xc[i] <- x + (i - 1) * w
    }
    w <- xc[n] +
      (strwidth(val[n], units = "user", cex = val_cex, font = 1) / 2) -
      xc[1] -
      (strwidth(val[1], units = "user", cex = val_cex, font = 1) / 2)
  }
  h <- max(strheight(val, units = "user", cex = val_cex, font = 1))
  return(list(x = xc, y = yc, w = w, h = h))
}


val_cont <- function(val, val_rnd) {
  if (length(val) == 2) {
    val_ref_s <- pretty(val, n = 5)
    val_ref <- c(val_ref_s[val_ref_s > min(val) & val_ref_s < max(val)])
  } else if (length(val) > 2) {
    val_ref <- val
  } else {
    stop("You need to provide at least two values for 'val'", call. = FALSE)
  }
  indices <- round((val_ref - min(val)) / (max(val) - min(val)) * 100, 0) + 1
  val_ref <- get_val_rnd(val_ref, val_rnd)
  vval <- rep("", 101)
  vval[indices] <- val_ref
  vval
}
