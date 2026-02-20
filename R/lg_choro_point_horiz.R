leg_choro_point_h <- function(pos = "left",
                              val,
                              pal = "Inferno",
                              alpha = NULL,
                              symbol = "circle",
                              title = "Legend Title",
                              title_cex = .8 * size,
                              val_cex = .6 * size,
                              val_rnd = 0,
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
                              cex = 1,
                              return_bbox = FALSE,
                              adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # boxes sizes
  w_box <- cex * size * x_spacing * 5 * 2 / 3
  h_box <- cex * size * y_spacing * 5 * 2 / 3


  # Nb. boxes and values
  n_val <- length(val)
  n_box <- n_val - 1
  if (n_val < 2) {
    stop("You need to provide at least two values for 'val'", call. = FALSE)
  }

  # rounded and ordered values for the legend
  val <- get_val_rnd(val = val, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)

  # box colors
  pal <- get_pal(pal, n_box, alpha = alpha)

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # label dimension
  labels_dim <- list(
    w = strwidth(val[1], units = "user", cex = val_cex, font = 1) / 2 +
      strwidth(val[n_val], units = "user", cex = val_cex, font = 1) / 2 +
      n_box * w_box,
    h = max(strheight(val, units = "user", cex = val_cex, font = 1))
  )

  # NA box and label dimensions
  if (isTRUE(no_data)) {
    na_label_dim <- list(
      w = max(strwidth(no_data_txt, units = "user", cex = val_cex, font = 1), w_box),
      h = strheight(no_data_txt, units = "user", cex = val_cex, font = 1)
    )
  } else {
    na_label_dim <- list(w = 0, h = 0)
    no_data_txt <- ""
  }

  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        labels_dim$w + ifelse(na_label_dim$w != 0, na_label_dim$w + 2 * x_spacing * size, 0)
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      h_box +
      y_spacing +
      max(labels_dim$h, na_label_dim$h) +
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
  top <- rep(
    legend_coords$top - x_spacing -
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0),
    n_box
  )
  bottom <- top - h_box
  left <- rep(NA, n_box)
  x <- legend_coords$left + x_spacing + strwidth(val[1], units = "user", cex = val_cex, font = 1) / 2
  for (i in 1:n_box) {
    left[i] <- x + (i - 1) * w_box
  }
  right <- left + w_box

  if (symbol == "square") {
    rect(
      xleft = left, ybottom = bottom, xright = right, ytop = top,
      col = pal, border = border, lwd = .7
    )
  }

  if (symbol == "circle") {
    symbols(
      x = left + (right - left) / 2, y = bottom + (top - bottom) / 2,
      circles = (right - left) / 2, inches = FALSE, add = TRUE,
      bg = pal, fg = border, lwd = .7
    )
  }

  # display labels
  y <- rep(bottom[1] - y_spacing, n_val)
  x <- rep(x, n_val)
  for (i in 1:n_val) {
    x[i] <- x[1] + (i - 1) * w_box
  }
  text(x = x, y = y, labels = val, cex = val_cex, adj = c(0.5, 1), col = fg)


  if (isTRUE(no_data)) {
    # display na box
    bottom <- bottom[1]
    top <- top[1]
    center <- legend_coords$right - y_spacing - na_label_dim$w / 2
    if (symbol == "square") {
      rect(
        xleft = center - w_box / 2, ybottom = bottom,
        xright = center + w_box / 2, ytop = top,
        col = col_na, border = border, lwd = .7
      )
    }
    if (symbol == "circle") {
      symbols(
        x = center, y = bottom + (top - bottom) / 2,
        circles = (right[1] - left[1]) / 2, inches = FALSE, add = TRUE,
        bg = col_na, fg = border, lwd = .7
      )
    }

    # display na label
    text(
      x = center, y = y[1], labels = no_data_txt, cex = val_cex,
      adj = c(0.5, 1), col = fg
    )
  }


  return(invisible(NULL))
}
