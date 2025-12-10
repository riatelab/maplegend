#' Plot a legend for a symbols map
#' @description This function can plot a legend for a symbols maps.
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
#' @param border type = "prop": color of the symbols borders
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param cex cex of the symbols
#' @param pch pch of the symbols (0:25)
#' @param cex_na cex of the symbols for missing values
#' @param pch_na pch of the symbols for missing values
#' @param lwd width of the symbols borders,
#' @param box_cex width and height cex of boxes
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
#' leg_symb(
#'   val = c("Type C", "Type D"), pal = c("cyan", "plum"),
#'   pch = c(21, 23), cex = c(1, 2)
#' )
leg_symb <- function(pos = "left",
                     val,
                     pal = "Inferno",
                     alpha = NULL,
                     pch = seq_along(val),
                     cex = rep(1, length(val)),
                     border = "#333333",
                     lwd = .7,
                     title = "Legend title",
                     title_cex = .8 * size,
                     val_cex = .6 * size,
                     cex_na = 1,
                     pch_na = 4,
                     col_na = "white",
                     no_data = FALSE,
                     no_data_txt = "No Data",
                     frame = FALSE,
                     frame_border = fg,
                     bg = "#f7f7f7",
                     fg = "#333333",
                     size = 1,
                     box_cex = c(1, 1),
                     return_bbox = FALSE,
                     adj = c(0, 0)) {
  # spacings
  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4

  # n. symbols
  n_val <- length(val)

  # symbol color
  col <- pbg <- get_pal(pal, n_val, alpha = alpha)
  if (any(pch %in% 21:25)) {
    col[pch %in% 21:25] <- border
  }
  if (no_data) {
    col_na_bg <- col_na
    col_na[pch_na %in% 21:25] <- border[1]
  }

  # symbols attributes
  if (length(pch) == 1) {
    pch <- rep(pch, n_val)
  }
  if (length(cex) != n_val) {
    cex <- rep(cex[1], n_val)
  }
  if (length(lwd) != n_val) {
    lwd <- rep(lwd[1], n_val)
  }

  # symbol sizes
  symb_sizes <- list(w = rep(NA, n_val), h = rep(NA, n_val))
  for (i in seq_len(n_val)) {
    symb_sizes$w[i] <- strwidth("M", units = "user", cex = cex[i]) * .75
    symb_sizes$h[i] <- strheight("M", units = "user", cex = cex[i]) * .75
  }

  # title dimensions
  title_dim <- get_title_dim(title, title_cex)

  # label dimension
  labels_dim <- list(
    w = max(strwidth(val, units = "user", cex = val_cex, font = 1))
  )

  # label (+) box dim
  max_sizes <- pmax(
    strheight(val, units = "user", cex = val_cex, font = 1),
    symb_sizes$h
  )

  # NA box and label dimensions
  if (isTRUE(no_data)) {
    na_box_dim <- list(
      w = strwidth("M", units = "user", cex = cex_na) * .75,
      h = strheight("M", units = "user", cex = cex_na) * .75
    )
    na_label_dim <- list(
      w = strwidth(no_data_txt, units = "user", cex = val_cex, font = 1),
      h = max(strheight(no_data_txt, units = "user", cex = val_cex, font = 1), na_box_dim$h)
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
        max(symb_sizes$w) + labels_dim$w + x_spacing,
        max(symb_sizes$w) + na_label_dim$w + x_spacing
      ) +
      x_spacing,
    h = y_spacing +
      ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) +
      sum(max_sizes) + (n_val - 1) * y_spacing +
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


  center_h <- rep(NA, n_val)
  center_h[1] <- legend_coords$top - y_spacing -
    ifelse(title_dim$h != 0, title_dim$h + 2 * y_spacing * size, 0) -
    max_sizes[1] / 2
  if (n_val > 1) {
    for (i in 2:n_val) {
      center_h[i] <- center_h[i - 1] - max_sizes[i - 1] / 2 - y_spacing - max_sizes[i] / 2
    }
  }
  center_w <- rep(legend_coords$left + x_spacing + max(symb_sizes$w, na_box_dim$w) / 2, n_val)
  for (i in seq_len(n_val)) {
    points(
      x = center_w[i],
      y = center_h[i],
      col = col[i],
      pch = pch[[i]],
      cex = cex[i],
      bg = pbg[i],
      lwd = lwd[i]
    )
  }
  text(
    x = center_w + x_spacing + max(symb_sizes$w, na_box_dim$w) / 2,
    y = center_h,
    labels = val,
    cex = val_cex,
    adj = c(0, 0.5),
    col = fg
  )

  if (isTRUE(no_data)) {
    # display na box
    bottom <- legend_coords$bottom + y_spacing + na_label_dim$h / 2
    points(
      x = center_w[i],
      y = bottom,
      col = col_na,
      pch = pch_na,
      cex = cex_na,
      bg = col_na_bg,
      lwd = lwd
    )

    # display na label
    text(
      x = center_w[1] + x_spacing + max(symb_sizes$w, na_box_dim$w) / 2,
      y = bottom,
      labels = no_data_txt,
      cex = val_cex,
      adj = c(0, 0.5),
      col = fg
    )
  }


  return(invisible(NULL))
}
