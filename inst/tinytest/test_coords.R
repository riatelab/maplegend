plot.new()
plot.window(xlim = c(0, 1),
            ylim = c(0, 1))
box()
vbox <- vector("list", 16)
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

set.seed(46)
x <- round(rnorm(1000)*1000, 2)
xh <- hist(x, breaks = quantile(x, 0:10 /10), plot = F)
vbox[[11]] <-
  leg(
    type = "histo",
    val = xh,
    pos = "top",
    return_bbox = TRUE
  )

vbox[[12]] <-
  leg(
    type = "choro_point",
    val = 15:20,
    pos = "top",
    return_bbox = TRUE
  )
vbox[[13]] <-
  leg(
    type = "choro_point",
    val = 15:20,
    pos = "top", horiz = TRUE,
    return_bbox = TRUE
  )

vbox[[14]] <-
  leg(
    type = "choro_line",
    val = 15:20,
    pos = "top",
    return_bbox = TRUE
  )

vbox[[15]] <-
  leg(
    type = "typo_line",
    val = 15:20,
    pos = "top",
    return_bbox = TRUE
  )

vbox[[16]] <-
  leg(
    type = "choro_symb",
    val = 15:20,
    pos = "top",
    return_bbox = TRUE
  )
# saveRDS(vbox,
#         file = '/home/tim/Documents/pkg/maplegend/inst/tinytest/coords.rds')

vbox_ref <- readRDS('coords.rds')
expect_equal(vbox, vbox_ref)
