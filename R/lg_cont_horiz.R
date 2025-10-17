leg_cont_h <- function(pos = "left",
                       val,
                       pal = "Inferno",
                       alpha = NULL,
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
                       box_border = "#333333",
                       bg = "#f7f7f7",
                       fg = "#333333",
                       size = 1,
                       box_cex = c(1, 1),
                       horiz = FALSE,
                       return_bbox = FALSE,
                       adj = c(0, 0)) {
  box_cex <- eval(box_cex)
  pal <- eval(pal)

  if (length(pal) != 1) {
    pal <- grDevices::colorRampPalette(colors = pal, alpha = TRUE)(100)
  } else {
    pal <- grDevices::hcl.colors(n = 100, palette = pal, rev = TRUE, alpha = alpha)
  }
  val <- val_cont(val, val_rnd)

  box_cex[1] <- box_cex[1] * .05

  leg_choro_h(
    val = val, pos = pos, pal = pal,
    title = title, title_cex = title_cex, alpha = alpha,
    val_cex = val_cex, val_rnd = val_rnd, col_na = col_na,
    no_data = no_data,
    no_data_txt = no_data_txt, frame = frame, box_border = NA,
    bg = bg, fg = fg, val_dec = val_dec, val_big = val_big,
    size = size, box_cex = box_cex,
    frame_border = frame_border,
    return_bbox = return_bbox, adj = adj
  )
}
