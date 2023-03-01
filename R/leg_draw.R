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
#' @return
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

  if (pos == "bottomright") {
    xleg <- min(unlist(lapply(dimleg, function(x) {
      x$xleft
    })))
    xleg <- rep(xleg, length(dimleg))
    sizes <- unlist(lapply(dimleg, function(x) {
      x$ytop - x$ybottom
    }))
    ybottominit <-
      min(unlist(lapply(dimleg, function(x) {
        x$ybottom
      })))
    yleg  <- ybottominit + rev(cumsum(rev(sizes)))

    frame_c <- c(
      xmin = xleg[1],
      xmax = min(unlist(lapply(dimleg, function(x) {
        x$xright
      }))),
      ymin = min(unlist(lapply(dimleg, function(x) {
        x$ybottom
      }))),
      ymax = yleg[1]
    )
  }

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
