leg_choro_symb <- function(pos = "left",
                           val,
                           pal = "Inferno",
                           alpha = NULL,
                           pch = 21,
                           cex = 1,
                           lwd = .7,
                           title = "Legend Title",
                           title_cex = .8 * size,
                           val_cex = .6 * size,
                           val_rnd = 2,
                           val_dec = ".",
                           val_big = "",
                           col_na = "white",
                           no_data = FALSE,
                           no_data_txt = "No Data",
                           frame = FALSE,
                           frame_border = fg,
                           border = "#333333",
                           bg = "#f7f7f7",
                           fg = "#333333",
                           size = 1,
                           box_cex = c(1, 1),
                           return_bbox = FALSE,
                           adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # Nb. boxes and values
  n_val <- length(val)
  n_box <- n_val - 1
  if (n_val < 2) {
    stop("You need to provide at least two values for 'val'", call. = FALSE)
  }

  # rounded and ordered values for the legend
  val <- rev(get_val_rnd(
    val = val, val_rnd = val_rnd, val_dec = val_dec,
    val_big = val_big
  ))

  # symbols attributes
  pch <- pch[[1]]
  cex <- cex[[1]]
  lwd <- lwd[[1]]
  border <- border[[1]]

  # symbol color
  col <- pbg <- rev(get_pal(pal, n_box, alpha = alpha))
  if (pch %in% 21:25) {
    col <- rep(border, n_box)
  }
  if (no_data) {
    col_na_bg <- col_na
    if (pch %in% 21:25) {
      col_na <- border
    }
  }

  # symbol sizes
  symb_sizes <- list(
    w =  strwidth("M", units = "user", cex = cex) * .9 * box_cex[1],
    h = strheight("M", units = "user", cex = cex) * .9 * box_cex[2]
  )

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # label dimension
  labels_dim <- list(
    w = max(strwidth(val, units = "user", cex = val_cex, font = 1))
  )

  # label dimension
  labels_dim <- list(
    w = max(strwidth(val, units = "user", cex = val_cex, font = 1)),
    h = strheight(val[1], units = "user", cex = val_cex, font = 1) / 2 +
      strheight(val[n_val], units = "user", cex = val_cex, font = 1) / 2 +
      n_box * symb_sizes$h
  )

  # NA box and label dimensions
  if (isTRUE(no_data)) {
    na_box_dim <- symb_sizes
    na_label_dim <- list(
      w = strwidth(no_data_txt, units = "user", cex = val_cex, font = 1),
      h = max(strheight(no_data_txt, units = "user", cex = val_cex, font = 1), na_box_dim$h)
    )
  } else {
    na_box_dim <- list(w = 0, h = 0)
    na_label_dim <- list(w = 0, h = 0)
    no_data_txt <- ""
  }

  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        symb_sizes$w + labels_dim$w + x_spacing,
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
  right <- left + symb_sizes$w
  y <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    strheight(val[1], units = "user", cex = val_cex, font = 1) / 2
  top <- rep(NA, n_box)
  for (i in 1:n_box) {
    top[i] <- y - (i - 1) * symb_sizes$h
  }
  bottom <- top - symb_sizes$h

  for (i in seq_len(n_val)) {
    points(
      x = left[i] + (right[i] - left[i]) / 2,
      y = bottom[i] + (top[i] - bottom[i]) / 2,
      col = col[i],
      pch = pch,
      cex = cex,
      bg = pbg[i],
      lwd = lwd
    )
  }
  # display labels
  x <- rep(legend_coords$left + x_spacing + symb_sizes$w + x_spacing, n_val)
  y <- rep(NA, n_val)
  top <- top[1]
  for (i in 1:n_val) {
    y[i] <- top - (i - 1) * symb_sizes$h
  }
  text(x = x, y = y, labels = val, cex = val_cex, adj = c(0, 0.5), col = fg)

  if (isTRUE(no_data)) {
    # display na box
    xna <- legend_coords$left + x_spacing + symb_sizes$w / 2
    yna <- legend_coords$bottom + y_spacing + na_label_dim$h / 2
    points(
      x = xna,
      y = yna,
      col = col_na,
      pch = pch,
      cex = cex,
      bg = col_na_bg,
      lwd = lwd
    )


    # display na label
    text(
      x = legend_coords$left + x_spacing + symb_sizes$w + x_spacing,
      y = yna,
      labels = no_data_txt,
      cex = val_cex,
      adj = c(0, 0.5),
      col = fg
    )
  }


  return(invisible(NULL))
}
