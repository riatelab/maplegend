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

vbox_ref <-list(list(right = 0.0985930232558139, left = -0.102790697674419,
                     top = 1.04, bottom = 0.844380813953488), list(right = 0.0781104651162791,
                                                                   left = -0.102790697674419, top = 0.176877906976744, bottom = -0.04),
                list(right = 0.590450581395349, left = 0.409549418604651,
                     top = 1.04, bottom = 0.851686046511628), list(right = 1.10279069767442,
                                                                   left = 0.93156976744186, top = 1.04, bottom = 0.899982558139535),
                list(right = 0.590450581395349, left = 0.409549418604651,
                     top = 0.12368023255814, bottom = -0.04), list(right = 1.10279069767442,
                                                                   left = 0.921889534883721, top = 0.141994186046512, bottom = -0.04),
                list(right = 1.10279069767442, left = 0.921889534883721,
                     top = 1.04, bottom = 0.788238372093023), list(right = 0.193162943137169,
                                                                   left = -0.102790697674419, top = 1.04, bottom = 0.826610465116279),
                list(right = 0.359372093023256, left = -0.102790697674419,
                     top = 0.552625, bottom = 0.447375), list(right = 0.441395348837209,
                                                              left = -0.102790697674419, top = 0.06525, bottom = -0.04))
expect_equal(vbox, vbox_ref)
