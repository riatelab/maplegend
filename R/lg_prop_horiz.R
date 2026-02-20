leg_prop_h <- function(pos = "left",
                       val,
                       col = "tomato4",
                       alpha = NULL,
                       inches = .3,
                       val_max = NULL,
                       symbol = "circle",
                       border = "#333333",
                       lwd = .7,
                       title = "Legend Title",
                       title_cex = .8 * size,
                       val_cex = .6 * size,
                       val_rnd = 0,
                       val_dec = ".",
                       val_big = "",
                       frame = FALSE,
                       frame_border = fg,
                       bg = "#f7f7f7",
                       fg = "#333333",
                       size = 1,
                       box_cex = 1,
                       self_adjust = FALSE,
                       return_bbox = FALSE,
                       adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # color mgmt
  col <- ifelse(!is.null(alpha), get_hex_pal(col, alpha), col)
  border <- border[[1]]
  lwd <- lwd[[1]]

  # values & values labels
  val <- unique(val)
  if (self_adjust == TRUE) {
    val <- self_adjust_v(val, inches, val_cex)
  }
  val <- sort(val, decreasing = TRUE)
  n_val <- length(val)
  valleg <- get_val_rnd(val = val, val_rnd = val_rnd, val_dec = val_dec, val_big = val_big)

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # adjust max_val
  if (is.null(val_max)) {
    val_max <- max(val)
  }

  # largest symbol size
  symb_sizes <- list(inches = sqrt(val * inches * inches / val_max))
  symb_sizes$x <- xinch(symb_sizes$inches)
  symb_sizes$y <- yinch(symb_sizes$inches)
  inches <- max(symb_sizes$inches)
  symb_dim <- list(
    w = sum(symb_sizes$x) + (n_val - 1) * x_spacing,
    h = yinch(inches * 2)
  )

  # label dimension
  labels_dim <- list(
    h = strheight(valleg[1], units = "user", cex = val_cex, font = 1)
  )

  # label (+) symb dim
  max_sizes <- pmax(
    strwidth(valleg, units = "user", cex = val_cex, font = 1),
    symb_sizes$x * 2
  )

  # legend dimension
  legend_dim <- list(
    w = x_spacing +
      max(
        title_dim$w,
        sum(max_sizes) + x_spacing * (n_val - 1)
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      symb_dim$h + y_spacing + labels_dim$h +
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

  # display symbols & lines
  pal <- rep(col, n_val)

  x <- rep(NA, n_val)
  x[1] <- legend_coords$left + x_spacing + max_sizes[1] / 2
  if (n_val > 1) {
    for (i in 2:n_val) {
      x[i] <- x[i - 1] + max_sizes[i - 1] / 2 + x_spacing + max_sizes[i] / 2
    }
  }
  y <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    symb_sizes$y[1] * 2 + symb_sizes$y
  if (symbol == "circle") {
    symbols(
      x = x,
      y = y,
      circles = symb_sizes$inches,
      bg = pal,
      fg = border,
      lwd = lwd,
      add = TRUE,
      inches = inches,
    )
    # display labels
    text(
      x = x,
      y = legend_coords$bottom + y_spacing,
      labels = valleg, cex = val_cex, adj = c(0.5, 0), col = fg
    )
  }
  if (symbol == "square") {
    symbols(
      x = x,
      y = y,
      squares = symb_sizes$inches,
      bg = pal,
      fg = border,
      lwd = lwd,
      add = TRUE,
      inches = inches * 2,
    )
    # display labels
    text(
      x = x,
      y = legend_coords$bottom + y_spacing,
      labels = valleg, cex = val_cex, adj = c(0.5, 0), col = fg
    )
  }

  return(invisible(NULL))
}
