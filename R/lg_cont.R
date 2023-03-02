#' @title Plot a legend for a choropleth map
#' @description This function plots a legend for a choropleth map.
#'
#' @param pal a set of colors.
#' @param col_na color for missing values
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y))
#' @param val break labels
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param box_border color of the boxes' borders
#' @param size size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param box_cex width and height cex of boxes
#' @param mar plot margins
#' @param return_bbox return only bounding box of the legend.
#' No legend is plotted.
#' @param adj adj
#' @keywords internal
#' @noRd
#' @import graphics
#' @return No return value, a legend is displayed.
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' leg_choro(val = c(1, 2, 3, 4), pal = c("red1", "red3", "red4"))
leg_cont <- function(pos = "left",
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
                     box_cex = c(1,1),
                     horiz = FALSE,
                     return_bbox = FALSE,
                     mar = par("mar"),
                     adj = c(0,0)) {

  box_cex <- eval(box_cex)
  pal <- eval(pal)
  if(length(pal) != 1){
    pal <- grDevices::colorRampPalette(colors = c(pal))(50)
  }else{
    pal <- hcl.colors(n = 50, palette = pal)
  }
  val <- seq(val[1], val[length(val)], length.out = 6)
  val <- get_val_rnd(val, val_rnd)

  vval <- rep("", 51)
  vval[c(1, 11, 21, 31, 41, 51)] <- val

  box_cex[1] <- box_cex[1] *.4
  box_cex[2] <- box_cex[2] * .1

  leg(type = "choro", val = vval, pos = pos, pal = pal, title = title, title_cex = title_cex,
      val_cex = val_cex, val_rnd = val_rnd, col_na = col_na, no_data = no_data,
      no_data_txt = no_data_txt, frame = frame, box_border = NA, bg = bg, fg = fg,
      size = size, box_cex = box_cex, horiz = horiz,
      return_bbox = return_bbox, mar = mar, adj = adj)
}

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
                     box_cex = c(1,1),
                     horiz = FALSE,
                     return_bbox = FALSE,
                     mar = par("mar"),
                     adj = c(0,0)) {

  box_cex <- eval(box_cex)
  pal <- eval(pal)
  if(length(pal) != 1){
    pal <- grDevices::colorRampPalette(colors = c(pal))(50)
  }else{
    pal <- hcl.colors(n = 50, palette = pal)
  }
  val <- seq(val[1], val[length(val)], length.out = 6)
  val <- get_val_rnd(val, val_rnd)

  vval <- rep("", 51)
  vval[c(1, 11, 21, 31, 41, 51)] <- val

  box_cex[1] <- box_cex[1] * .05
  box_cex[2] <- box_cex[2]



  leg(type = "choro", val = vval, pos = pos, pal = pal,
      title = title, title_cex = title_cex,
      val_cex = val_cex, val_rnd = val_rnd, col_na = col_na,
      no_data = no_data,
      no_data_txt = no_data_txt, frame = frame, box_border = NA,
      bg = bg, fg = fg,
      size = size, box_cex = box_cex, horiz = TRUE,
      return_bbox = return_bbox, mar = mar, adj = adj)
}
