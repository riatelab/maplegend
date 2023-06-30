leg_cont_h <- function(pos = "left",
                       val,
                       pal = "Plasma",
                       title = "Legend Title",
                       title_cex = .8 * size,
                       val_cex = .6 * size,
                       val_rnd = 0,
                       col_na = "white",
                       no_data = FALSE,
                       no_data_txt = "No Data",
                       frame = FALSE,
                       box_border = "#333333",
                       bg = "#f7f7f7",
                       fg = "#333333",
                       size = 1,
                       box_cex = c(1, 1),
                       horiz = FALSE,
                       return_bbox = FALSE,
                       mar = par("mar"),
                       adj = c(0, 0)) {
  box_cex <- eval(box_cex)
  pal <- eval(pal)
  if (length(pal) != 1) {
    pal <- grDevices::colorRampPalette(colors = c(pal))(100)
  } else {
    pal <- hcl.colors(n = 100, palette = pal, rev = TRUE)
  }
  val <- val_cont(val, val_rnd)

  box_cex[1] <- box_cex[1] * .05
  box_cex[2] <- box_cex[2]

  leg(
    type = "choro", val = val, pos = pos, pal = pal,
    title = title, title_cex = title_cex,
    val_cex = val_cex, val_rnd = val_rnd, col_na = col_na,
    no_data = no_data,
    no_data_txt = no_data_txt, frame = frame, box_border = NA,
    bg = bg, fg = fg,
    size = size, box_cex = box_cex, horiz = TRUE,
    return_bbox = return_bbox, mar = mar, adj = adj
  )
}
