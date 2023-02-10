#
#
#
#
#
#
#
#
#
# leg_a <- function(leg, type, ...) {
#   res <- c(as.list(environment()), list(...))
#   res <- clean_input(res, type = type)
#   res$bbox <- TRUE
#   if (missing(leg)) {
#     leg <- list()
#   }
#   leg$layers[[length(leg$layers) + 1]] <- res
#   return(leg)
# }
#
#
#
# clean_input <- function(res, type) {
#   res <- res[unlist(lapply(
#     X = res,
#     FUN = function(x) {
#       !is.name(x)
#     }
#   ))]
#   res$leg <- NULL
#   res$type <- type
#   res
# }
#
#
# leg_draw <- function(x, pos, bg) {
#   dimleg <- list()
#
#
#   if(missing(bg)){
#     bgs <- sapply(x$layers, function(x)x$bg)
#     bgs <- bgs[!sapply(bgs, is.null)]
#     if(length(bgs)>=1){
#       bg <- bgs[[1]]
#     }else{
#       bg <- NA
#     }
#   }
#
#
#   for (i in 1:length(x$layers)) {
#     x$layers[[i]]$pos <- pos
#     x$layers[[i]]$bbox <- TRUE
#     dimleg[[i]] <- do.call(leg, x$layers[[i]])
#   }
#
#
#
#
#   if (pos == "bottomright") {
#     xleg <- min(unlist(lapply(dimleg, function(x) {
#       x$xleft
#     })))
#     xleg <- rep(xleg, length(dimleg))
#     sizes <- unlist(lapply(dimleg, function(x) {
#       x$ytop - x$ybottom
#     }))
#     ybottominit <-
#       min(unlist(lapply(dimleg, function(x) {
#         x$ybottom
#       })))
#     yleg  <- ybottominit + rev(cumsum(rev(sizes)))
#
#
#
#     frame_c <- c(xmin = xleg[1],
#                  xmax = min(unlist(lapply(dimleg, function(x){x$xright}))),
#                  ymin = min(unlist(lapply(dimleg, function(x){x$ybottom}))),
#                  ymax = yleg[1])
#
#
#
#   }
#
#
#   frame <- any(sapply(x$layers, function(x)x$frame))
#   if(frame){
#     rect(xleft = frame_c[1], ybottom = frame_c[3],
#          xright = frame_c[2], ytop = frame_c[4],
#          col = bg, border = "green", xpd = TRUE)
#
#   }
#
#
#
#
#
#
#
#   for (i in 1:length(x$layers)) {
#     x$layers[[i]]$pos <- c(xleg[i], yleg[i])
#     x$layers[[i]]$bbox <- FALSE
#     x$layers[[i]]$frame <- FALSE
#     do.call(leg, x$layers[[i]])
#   }
#
#   return(invisible(NULL))
# }
#
#
#
# library(mapsf)
# library(maplegend)
# m <- mf_get_mtq()
# mf_theme(bg = "yellow")
# mf_map(m)
# leg_a(
#   type = "prop",
#   val = c(1, 50, 80, 100),
#   col = "red",
#   inches = .2,
#   frame = T,lwd = 2,
#   cex = 1,
#   symbol = "circle",
#   title = "POPOPOPOPO",
#    mar = mf_theme()$mar
# ) |>
#   leg_a(
#     type = "choro",
#     val = c(1, 2, 3, 100, 85502),
#     pal = 'Reds',
#     frame = T,
#     cex = 1,
#     bg= "green") |>
#   leg_a(
#     type = "typo",
#     val = c(1, 2, 3),
#     pal = c("red1", "red3", "red4"),
#     frame = T,
#     cex = 1
#   ) |>
#   leg_draw(pos = 'bottomright', bg ="lightblue")
# mf_scale()
#
# ll <- leg_a(
#   type = "prop",
#   val = c(1, 50, 80, 100),
#   col = "red",
#   inches = .2,
#   frame = T,lwd = 2,
#   cex = 1,
#   symbol = "circle",
#   title = "POPOPOPOPO",
#   mar = mf_theme()$mar
# )
#
# ll <- leg_a(ll,
#     type = "choro",
#     val = c(1, 2, 3, 100, 85502),
#     pal = 'Reds',
#     frame = T,
#     cex = 1,
#     bg= "green")
#
# ll <- leg_a(ll, type = "typo",
#     val = c(1, 2, 3),
#     pal = c("red1", "red3", "red4"),
#     frame = T,
#     cex = 1
#   )
#
#
# ll
#
# leg_draw(ll, pos = 'bottomright', bg ="lightblue")
#
# f
# mf_map(m)
# leg(type = "prop", pos = "bottomright2",
#        val = c(1,2,5,100), self_adjust = T, inches = .2, mar = getOption("mapsf.mar"), frame = T)
# leg_prop(val = c(1, 20, 100), col = "red", inches = .3)
#
# ?leg_prop
# ??leg
