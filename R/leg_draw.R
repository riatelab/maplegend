#' @title Draw a map legend
#' @description Draw a map legend with several elements.
#' @param x legend
#' @param pos pos
#' @param size size
#' @param bg bg
#' @param fg fg
#' @param frame frame
#' @param mar mar
#'
#' @return smthing is returned
#' @export
#'
#' @examples
#' plot(1:10)
leg_draw <- function(x,
                     pos = "bottomright",
                     size = 1,
                     bg = 'white',
                     fg = 'black',
                     frame = TRUE,
                     mar = par("mar")) {
  # plot.new()
  # plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  # x <-   leg_comp(type = "prop", val = c(10,50,100)) |>
  #   leg_comp(type = "typo", val = c("A", "B", "C"))
  # pos = "left"
  # mar = par("mar")
  # frame = TRUE
  # size = 1
  # bg = 'white'
  #   fg = 'black'
  dimleg <- list()
  insetf <- strwidth("MM", units = "user", cex = 1) / 4

  for (i in 1:length(x$layers)) {
    x$layers[[i]]$pos <- pos
    x$layers[[i]]$return_bbox <- TRUE
    x$layers[[i]]$mar <- mar
    x$layers[[i]]$size <- size
    x$layers[[i]]$frame <- frame
    dimleg[[i]] <- do.call(leg, x$layers[[i]])
  }

  res <- get_pos_and_frame(pos = pos, dimleg = dimleg)
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


  for (i in 1:length(x$layers)) {
    x$layers[[i]]$mar <- mar
    x$layers[[i]]$pos <- c(xleg[i], yleg[i]) + c(-(size-1)*insetf, (size-1)*insetf)
    x$layers[[i]]$return_bbox <- FALSE
    x$layers[[i]]$frame <- FALSE
    x$layers[[i]]$size <- size
    x$layers[[i]]$bg <- bg
    x$layers[[i]]$fg <- fg
    do.call(leg, x$layers[[i]])
  }

  return(invisible(NULL))
}





get_pos_and_frame <- function(pos, dimleg){

  xleft <- min(unlist(lapply(dimleg, function(x) {
    x$xleft
  })))
  xright <- max(unlist(lapply(dimleg, function(x) {
    x$xright
  })))
  width <- xright - xleft
  heights <- unlist(lapply(dimleg, function(x) {
    x$ytop - x$ybottom
  }))
  height <- sum(heights)

  xleg <- rep(xleft, length(dimleg))



  if (startsWith(pos, "top")){
    ytop <- max(unlist(lapply(dimleg, function(x) {x$ytop})))
    ybottom <- ytop - height
    yleg  <- ybottom + rev(cumsum(rev(heights)))
  }



  if (startsWith(pos, "bottom")){
    ybottom <- min(unlist(lapply(dimleg, function(x) {x$ybottom })))
    ytop <- ybottom + height
    yleg  <- ybottom + rev(cumsum(rev(heights)))
  }


  if (pos %in% c("left", "right")){
    ymid <- dimleg[[1]][[2]] + (dimleg[[1]][[4]] - dimleg[[1]][[2]]) / 2
    ytop <- ymid + height / 2
    ybottom <- ytop - height
    yleg  <- ybottom + rev(cumsum(rev(heights)))
  }



  frame_c <- c(
    xmin = xleft,
    xmax = xright,
    ymin = ybottom,
    ymax = ytop
  )

  return(list(xleg = xleg, yleg = yleg, frame_c = frame_c))
}
