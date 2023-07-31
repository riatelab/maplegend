#' @title Plot a composed map legend
#' @description Draw a map legend with several elements.
#' @param x list of legend parameters
#' @param pos position of the legend. . It can be one of 'topleft',
#' 'topright', 'right', 'bottomright', 'bottomleft' or 'left',
#' @param size size of the legend; 2 means two times bigger
#' @param bg background color of the legend
#' @param fg foreground color of the legend
#' @param frame if TRUE the legend is plotted within a frame
#' @param adj adjust the postion of the legend in x and y directions.
#' @param mar plot margins
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#'
#' @return A composed legend is plotted. Nothing is returned.
#' @export
#'
#' @examples
#' # minimal example
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' box()
#' leg_comp(type = "prop", val = c(10, 50, 100)) |>
#'   leg_comp(type = "typo", val = c("A", "B", "C")) |>
#'   leg_draw(pos = "topright", bg = "lightblue")
leg_draw <- function(x,
                     pos = "bottomright",
                     size = 1,
                     bg = "white",
                     fg = "black",
                     frame = TRUE,
                     title_cex = 0.8 * size,
                     val_cex = 0.6 * size,
                     adj = c(0, 0),
                     mar = par("mar")) {
  dimleg <- list()
  insetf <- strwidth("MM", units = "user", cex = 1) / 4


  if (any(is.na(pos))){return(invisible(NULL))}

  for (i in seq_along(x$layers)) {
    x$layers[[i]]$pos <- pos
    x$layers[[i]]$return_bbox <- TRUE
    x$layers[[i]]$mar <- mar
    x$layers[[i]]$size <- size
    x$layers[[i]]$frame <- frame
    x$layers[[i]]$adj <- adj
    x$layers[[i]]$title_cex <- title_cex
    x$layers[[i]]$val_cex <- val_cex

    dimleg[[i]] <- do.call(leg, x$layers[[i]])
  }


  res <- get_pos_and_frame(pos = pos, dimleg = dimleg, adj = adj)
  xleg <- res$xleg
  yleg <- res$yleg
  frame_c <- res$frame_c


  if (frame) {
    rect(
      xleft = frame_c[1],
      ybottom = frame_c[3],
      xright = frame_c[2],
      ytop = frame_c[4],
      col = bg,
      border = fg,
      xpd = TRUE
    )
  }


  for (i in seq_along(x$layers)) {
    x$layers[[i]]$mar <- mar
    x$layers[[i]]$pos <-
      c(xleg[i], yleg[i]) + c(-(size - 1) * insetf, (size - 1) * insetf)
    x$layers[[i]]$return_bbox <- FALSE
    x$layers[[i]]$frame <- FALSE
    x$layers[[i]]$bg <- bg
    x$layers[[i]]$fg <- fg
    x$layers[[i]]$adj <- c(0, 0)

    do.call(leg, x$layers[[i]])
  }

  return(invisible(NULL))
}





get_pos_and_frame <- function(pos, dimleg, adj) {
  xleft <- min(unlist(lapply(dimleg, function(x) {
    x$xleft
  })))
  xright <- max(unlist(lapply(dimleg, function(x) {
    x$xright
  })))
  heights <- unlist(lapply(dimleg, function(x) {
    x$ytop - x$ybottom
  }))
  height <- sum(heights)

  xleg <- rep(xleft, length(dimleg))



  if (startsWith(pos, "top")) {
    ytop <- max(unlist(lapply(dimleg, function(x) {
      x$ytop
    })))
    ybottom <- ytop - height
    yleg <- ybottom + rev(cumsum(rev(heights)))
  }



  if (startsWith(pos, "bottom")) {
    ybottom <- min(unlist(lapply(dimleg, function(x) {
      x$ybottom
    })))
    ytop <- ybottom + height
    yleg <- ybottom + rev(cumsum(rev(heights)))
  }


  if (pos %in% c("left", "right")) {
    ymid <- dimleg[[1]][[2]] + (dimleg[[1]][[4]] - dimleg[[1]][[2]]) / 2
    ytop <- ymid + height / 2
    ybottom <- ytop - height
    yleg <- ybottom + rev(cumsum(rev(heights)))
  }



  frame_c <- c(
    xmin = xleft,
    xmax = xright,
    ymin = ybottom,
    ymax = ytop
  )

  return(list(xleg = xleg, yleg = yleg, frame_c = frame_c))
}
