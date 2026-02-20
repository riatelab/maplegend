leg_choro_line <- function(pos = "topleft",
                           val,
                           pal = "Inferno",
                           alpha = NULL,
                           lwd = 2,
                           title = "Legend Title",
                           title_cex = .8 * size,
                           val_cex = .6 * size,
                           val_rnd = 2,
                           val_dec = ".",
                           val_big = "",
                           frame = FALSE,
                           frame_border = fg,
                           bg = "#f7f7f7",
                           fg = "#333333",
                           col_na = "grey80",
                           no_data = FALSE,
                           no_data_txt = "No Data",
                           size = 1,
                           box_cex = c(1, 1),
                           return_bbox = FALSE,
                           adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # boxes sizes
  w_box <- box_cex[1] * size * x_spacing * 5
  h_box <- box_cex[2] * size * y_spacing * 5 * 3 / 5

  # Nb. boxes and values
  n_val <- length(val)
  n_box <- n_val - 1
  if (n_val < 2) {
    stop("You need to provide at least two values for 'val'", call. = FALSE)
  }

  # rounded and ordered values for the legend
  val <- get_val_rnd(val = val, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)

  # color mgmt
  pal <- rev(get_pal(pal, n_box, alpha = alpha))

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # boxes dimensions
  boxes_dim <- list(w = w_box, h = n_box * h_box)

  # label dimension
  labels_dim <- list(
    w = max(strwidth(val, units = "user", cex = val_cex, font = 1)),
    h = strheight(val[1], units = "user", cex = val_cex, font = 1) / 2 +
      strheight(val[n_val], units = "user", cex = val_cex, font = 1) / 2 +
      n_box * h_box
  )

  # NA box and label dimensions
  if (isTRUE(no_data)) {
    na_box_dim <- list(w = w_box, h = h_box)
    na_label_dim <- list(
      w = strwidth(no_data_txt, units = "user", cex = val_cex, font = 1),
      h = max(
        strheight(no_data_txt, units = "user", cex = val_cex, font = 1),
        h_box
      )
    )
  } else {
    na_box_dim <- list(w = 0, h = 0)
    na_label_dim <- list(w = 0, h = 0)
    no_data_txt <- ""
  }


  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        boxes_dim$w + labels_dim$w + x_spacing,
        na_box_dim$w + na_label_dim$w + x_spacing
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      labels_dim$h +
      ifelse(na_label_dim$h != 0, na_label_dim$h + y_spacing * size, 0) +
      y_spacing
  )

  # get legend coordinates
  legend_coords <- get_legend_coords(
    pos = pos, legend_dim = legend_dim,
    adj = adj, frame = frame,
    x_spacing = x_spacing,
    y_spacing = y_spacing
  )

  # return legend coordinates only
  if (return_bbox) {
    return(invisible(legend_coords))
  }

  # display frame
  plot_frame(
    frame = frame, legend_coords = legend_coords,
    bg = bg, frame_border = frame_border,
    x_spacing = x_spacing, y_spacing = y_spacing
  )

  # display title
  plot_title(
    title = title, title_cex = title_cex, title_dim = title_dim,
    fg = fg, legend_coords = legend_coords,
    x_spacing = x_spacing, y_spacing = y_spacing
  )

  # display boxes
  left <- rep(legend_coords$left + x_spacing, n_box)
  right <- left + w_box
  y <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    strheight(val[1], units = "user", cex = val_cex, font = 1) / 2
  top <- rep(NA, n_box)
  for (i in 1:n_box) {
    top[i] <- y - (i - 1) * h_box
  }
  center <- top - h_box / 2
  segments(
    x0 = left, y0 = center, x1 = right, y1 = center, col = pal,
    lwd = lwd, lend = 1
  )

  # display labels
  x <- rep(legend_coords$left + x_spacing + w_box + x_spacing, n_val)
  y <- rep(NA, n_val)
  top <- top[1]
  for (i in 1:n_val) {
    y[i] <- top - (i - 1) * h_box
  }
  text(x = x, y = y, labels = rev(val), cex = val_cex, adj = c(0, 0.5), col = fg)

  if (isTRUE(no_data)) {
    # display na box
    left <- legend_coords$left + x_spacing
    right <- left + w_box
    bottom <- legend_coords$bottom + y_spacing +
      (na_label_dim$h - na_box_dim$h) / 2
    top <- bottom + h_box

    segments(
      x0 = left, y0 = bottom + h_box / 2, x1 = right, y1 = bottom + h_box / 2,
      col = col_na, lwd = lwd, lend = 1
    )

    # display na label
    x <- legend_coords$left + x_spacing + w_box + x_spacing
    y <- bottom + (top - bottom) / 2
    text(
      x = x, y = y, labels = no_data_txt, cex = val_cex, adj = c(0, 0.5),
      col = fg
    )
  }
  return(invisible(NULL))
}
