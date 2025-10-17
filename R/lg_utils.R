get_val_rnd <- function(val, val_rnd, val_dec = getOption("OutDec"), val_big = "") {
  if (is.numeric(val)) {
    val <- round(val, val_rnd)
    if (val_rnd <= 0) {
      val_rnd <- 0
    }
    val <- format(
      x = val, scientific = FALSE, nsmall = val_rnd,
      decimal.mark = val_dec, big.mark = val_big,
      trim = TRUE
    )
  }
  return(val)
}

get_pal <- function(pal, nbreaks, alpha = 1) {
  if (length(pal) == 1) {
    if (pal %in% grDevices::hcl.pals()) {
      cols <- grDevices::hcl.colors(n = nbreaks, palette = pal, rev = TRUE)
    } else {
      cols <- rep(pal, nbreaks)
    }
  } else {
    cols <- pal[1:nbreaks]
  }
  if (!is.null(alpha)) {
    cols <- get_hex_pal(cols, alpha)
  }

  return(cols)
}

get_hex_pal <- function(pal, alpha) {
  pal <- grDevices::col2rgb(pal, alpha = FALSE)
  ffun <- function(x) {
    grDevices::rgb(pal[1, x],
      pal[2, x],
      pal[3, x],
      maxColorValue = 255
    )
  }
  paste0(sapply(1:ncol(pal), ffun), get_alpha(alpha))
}

get_alpha <- function(alpha) {
  if (alpha < 0) {
    alpha <- 0
  }
  if (alpha > 1) {
    alpha <- 1
  }
  sprintf("%02X", as.integer(255.999 * alpha))
}

get_title_dim <- function(title, title_cex) {
  h <- strheight(s = title, units = "user", cex = title_cex, font = 1)
  w <- strwidth(s = title, units = "user", cex = title_cex, font = 1)
  if (title == "" || is.na(title)) {
    w <- h <- 0
  }
  return(list(w = w, h = h))
}

# get the position of the legend
get_legend_coords <- function(pos, legend_dim, adj, frame, x_spacing, y_spacing) {
  if (is.numeric(pos) && length(pos) == 2) {
    xy <- pos
  } else {
    pu <- par("usr")
    if (isTRUE(frame)) {
      adj <- adj + switch(pos,
        bottomleft = c(1, 1),
        topleft = c(1, -1),
        left = c(1, 0),
        top = c(0, -1),
        bottom = c(0, 1),
        bottomright = c(-1, 1),
        right = c(-1, 0),
        topright = c(-1, -1),
        interactive = c(0, 0)
      )
    }
    extra <- adj * c(x_spacing, y_spacing)
    xy <- switch(pos,
      bottomleft = c(
        pu[1],
        pu[3] + legend_dim$h
      ),
      topleft = c(
        pu[1],
        pu[4]
      ),
      left = c(
        pu[1],
        pu[3] + (pu[4] - pu[3]) / 2 + legend_dim$h / 2
      ),
      top = c(
        pu[1] + (pu[2] - pu[1]) / 2 - legend_dim$w / 2,
        pu[4]
      ),
      bottom = c(
        pu[1] + (pu[2] - pu[1]) / 2 - legend_dim$w / 2,
        pu[3] + legend_dim$h
      ),
      bottomright = c(
        pu[2] - legend_dim$w,
        pu[3] + legend_dim$h
      ),
      right = c(
        pu[2] - legend_dim$w,
        pu[3] + (pu[4] - pu[3]) / 2 + legend_dim$h / 2
      ),
      topright = c(
        pu[2] - legend_dim$w,
        pu[4]
      )
    )
    xy <- xy + extra
  }
  return(list(
    right = xy[1] + legend_dim$w,
    left = xy[1],
    top = xy[2],
    bottom = xy[2] - legend_dim$h
  ))
}


plot_title <- function(title, title_cex, title_dim, fg, legend_coords,
                       x_spacing, y_spacing) {
  if (title_dim$h != 0) {
    x <- legend_coords$left + x_spacing
    y <- legend_coords$top - y_spacing - title_dim$h
    text(x = x, y = y, labels = title, cex = title_cex, adj = c(0, 0), col = fg)
  }
  return(invisible(NULL))
}


plot_frame <- function(frame, legend_coords, bg, frame_border,
                       x_spacing, y_spacing) {
  if (isTRUE(frame)) {
    rect(
      xleft = legend_coords$left,
      xright = legend_coords$right,
      ytop = legend_coords$top,
      ybottom = legend_coords$bottom,
      col = bg,
      border = frame_border,
      lwd = .7
    )
  }
}

interleg <- function() {
  if (interactive()) {
    message(paste0("Click on the map to choose the legend position."))
    x <- unlist(locator(1))
    message(paste0("Legend coordinates:\nc(", x[[1]], ", ", x[[2]], ")"))
    return(x)
  } else {
    stop('You cannot use "interactive" in a non-interactive R session.',
      call. = FALSE
    )
  }
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


self_adjust_v <- function(var, inches, val_cex) {
  if (length(var) == 1) {
    return(var)
  }
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
  si <- yinch(sqrt(v * inches * inches / max(val)))

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
