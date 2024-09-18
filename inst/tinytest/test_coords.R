plot.new()
plot.window(xlim = c(0, 1),
            ylim = c(0, 1),
            asp = 1)
box()
vbox <- vector("list", 10)
vbox[[1]] <-
  leg(
    type = "prop",
    val = c(10, 50, 100),
    pos = "topleft",
    return_bbox = TRUE
  )
vbox[[2]] <- leg(
  type = "choro",
  val = c(10, 20, 30, 40, 50),
  pos = "bottomleft",
  return_bbox = TRUE
)
vbox[[3]] <- leg(
  type = "typo",
  val = c("A", "B", "C"),
  pos = "top",
  return_bbox = TRUE
)
vbox[[4]] <-
  leg(
    type = "symb",
    val = c("A", "B", "C"),
    pos = "topright",
    return_bbox = TRUE
  )
vbox[[5]] <-
  leg(
    type = "prop_line",
    val = c(5, 50, 100),
    pos = "bottom",
    lwd = 20,
    return_bbox = TRUE,
    box_cex = c(1, 1)
  )
vbox[[6]] <- leg(
  type = "grad_line",
  val = c(1, 4, 10, 15),
  pos = "bottomright",
  lwd = c(1, 5, 10),
  return_bbox = TRUE
)
vbox[[7]] <- leg(
  type = "cont",
  val = c(10, 20, 30, 40, 50),
  pos = "topright",
  return_bbox = TRUE
)
vbox[[8]] <-
  leg(
    type = "prop",
    val = c(10, 50, 100),
    pos = "topleft",
    horiz = TRUE,
    return_bbox = TRUE
  )
vbox[[9]] <-
  leg(
    type = "choro",
    val = c(10, 20, 30, 40, 50),
    pos = "left",
    horiz = TRUE,
    return_bbox = TRUE
  )
vbox[[10]] <-
  leg(
    type = "cont",
    val = c(10, 1000),
    pos = "bottomleft",
    horiz = TRUE,
    return_bbox = TRUE
  )
# dput(vbox)

vbox_ref <-
  list(list(xleft = -0.102790697674419, ybottom = 0.843558139534884,
            xright = 0.106720930232558, ytop = 1.04),
       list(xleft = -0.102790697674419,
            ybottom = -0.04, xright = 0.0781104651162791, ytop = 0.193142441860465),
       list(xleft = 0.420014534883721, ybottom = 0.794790697674419,
            xright = 0.600915697674419, ytop = 1.04),
       list(xleft = 0.93156976744186,
            ybottom = 0.814162790697674, xright = 1.10279069767442,
            ytop = 1.04),
       list(xleft = 0.420014534883721, ybottom = -0.04,
            xright = 0.600915697674419, ytop = 0.205209302325581),
       list(xleft = 0.921889534883721, ybottom = -0.04, xright = 1.10279069767442,
            ytop = 0.161622093023256),
       list(xleft = 0.921889534883721,
            ybottom = 0.768113372093023, xright = 1.10279069767442,
            ytop = 1.04),
       list(xleft = -0.102790697674419, ybottom = 0.831369186046512,
            xright = 0.178219571782624, ytop = 1.04),
       list(xleft = -0.102790697674419,
            ybottom = 0.434497093023256, xright = 0.405697674418605,
            ytop = 0.544572674418605),
       list(xleft = -0.102790697674419,
            ybottom = -0.04, xright = 0.499302325581395, ytop = 0.0700755813953488))

expect_equal(vbox, vbox_ref)
