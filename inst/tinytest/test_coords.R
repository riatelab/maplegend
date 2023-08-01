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
# print(dput(vbox))

vbox_ref <- list(
  list(
    xleft = -0.102790697674419,
    ybottom = 0.83543023255814,
    xright = 0.114848837209302,
    ytop = 1.04
  ),
  list(
    xleft = -0.102790697674419,
    ybottom = -0.04,
    xright = 0.0862383720930233,
    ytop = 0.201270348837209
  ),
  list(
    xleft = 0.420014534883721,
    ybottom = 0.786662790697675,
    xright = 0.609043604651163,
    ytop = 1.04
  ),
  list(
    xleft = 0.923441860465116,
    ybottom = 0.80603488372093,
    xright = 1.10279069767442,
    ytop = 1.04
  ),
  list(
    xleft = 0.420014534883721,
    ybottom = -0.04,
    xright = 0.609043604651163,
    ytop = 0.213337209302326
  ),
  list(
    xleft = 0.913761627906977,
    ybottom = -0.04,
    xright = 1.10279069767442,
    ytop = 0.155220930232558
  ),
  list(
    xleft = 0.913761627906977,
    ybottom = 0.759985465116279,
    xright = 1.10279069767442,
    ytop = 1.04
  ),
  list(
    xleft = -0.102790697674419,
    ybottom = 0.823241279069768,
    xright = 0.186347478759368,
    ytop = 1.04
  ),
  list(
    xleft = -0.102790697674419,
    ybottom = 0.425353197674419,
    xright = 0.413825581395349,
    ytop = 0.545588662790698
  ),
  list(
    xleft = -0.102790697674419,
    ybottom = -0.04,
    xright = 0.507430232558139,
    ytop = 0.0802354651162791
  )
)

expect_equal(vbox, vbox_ref)
