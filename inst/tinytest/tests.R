expect_error(leg(
  type = "grad_line",
  val = c(1, 4, 10, 15),
  pos = "bottomright",
  lwd = c(1, 5, 10),
  alpha = .5
))

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

expect_silent(leg(type = "prop", pos = "topright",
                  val = c(1, 5, 10), inches = .3))
expect_silent(leg(type = "prop", pos = "topright",
                  val = c(1, 5, 10), inches = .3, symbol = 'square'))
expect_silent(leg(type = "prop", pos = "topright", symbol = 'square',
                  val = c(1, 5, 10), inches = .3, horiz = TRUE))
expect_silent(leg(type = "choro", pos = "bottomright",
                  val = c(10, 20, 30, 40, 50),
                        pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "typo", pos = "topleft", val = c("A", "B", "C", "D"),
                        pal = hcl.colors(4, "Dynamic")))
expect_silent(leg(type = "symb", pos = "bottomleft",
                  val = c("A", "B", "C"),
                        pch = 21:23, cex = c(1, 2, 2),
                  pal = hcl.colors(3, "Dynamic")))
expect_silent(leg(type = "grad_line", pos = "top",
                  val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10)))
expect_silent(leg(type = "prop_line", pos = "bottom",
                  lwd = 20, val = c(5, 50, 100)))

expect_silent(leg(type = "prop", pos = "topright", val = c(2),
                  self_adjust = TRUE, horiz = TRUE, inches = .3))


expect_error(leg(type = "prop_line", pos = "interactive",
                 lwd = 20, val = c(5, 50, 100)))


expect_error(leg(type = "grad_line", pos = "NA",
                 val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10)))

expect_silent(leg(type = "grad_line",
                  pos = c(737788.682080213, 1628182.17278935),
                        val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10), frame = TRUE))
expect_silent(leg(type = "grad_line", pos = NA,
                        val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10)))

expect_silent(leg(type = "prop_line", pos = NA,
                        lwd = 20,
                        val = c(5, 50, 100)))
expect_silent(leg(type = "prop_line",
                  pos = c(737788.682080213, 1628182.17278935),
                        lwd = 20,
                        val = c(5, 50, 100),
                        frame = TRUE))

expect_silent(leg(type = "cont", pos = "top", val = c(10, 100)))

expect_silent(leg(type = "cont", pos = "top", val = c(10, 100), horiz = TRUE))
expect_silent(leg(type = "cont", pos = "top", val = c(10, 100), horiz = TRUE,
                  pal = c("red", "blue")))
expect_silent(leg(type = "cont", pos = "top", val = c(10, 100), horiz = FALSE,
                  pal = c("red", "blue")))
expect_silent(leg(type = "choro", pos = c(.5, .5),
                  frame = TRUE, no_data = TRUE, val = c(10, 20, 30, 40, 50),
                  pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "typo", pos = c(.5, .5),
                  frame = TRUE, no_data = TRUE, val = c(10, 20, 30, 40, 50),
                  pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "symb", pos = c(.1, .8),
                  frame = TRUE, no_data = TRUE, val = c(10, 20, 30, 40, 50),
                  pal = "Viridis", pch = 21:26))
expect_silent(leg(type = "symb", pos = c(.1, .8),
                  frame = TRUE, no_data = TRUE, val = c(10, 20, 30, 40, 50),
                  pal = "Viridis", pch = 21, cex = 1))
expect_silent(leg(type = "choro", pos = c(.5, .5), horiz = TRUE,
                  frame = TRUE, no_data = TRUE, val = c(10, 20, 30, 40, 50),
                  pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "prop", pos = c(.1, .8),
                  frame = TRUE, self_adjust = TRUE,
                  val = c(10, 20, 30, 40, 50),
                  col = 2))
expect_silent(leg(type = "prop", pos = c(.1, .8),
                  frame = TRUE, self_adjust = TRUE, horiz = TRUE,
                  val = c(10, 20, 30, 40, 50),
                  col = 2))

expect_silent(leg(type = "choro", pos = c(.5, .5),
                  frame = TRUE, no_data = TRUE,
                  val = c(10, 20, 30, 40, 50),
                  pal = hcl.colors(4, "Reds 2")))




plot.new()
plot.window(xlim = c(0, 1),
            ylim = c(0, 1),
            asp = 1)

expect_silent(leg(type = "symb", pos = c(.1, .8),
                  frame = TRUE, no_data = TRUE, val = c(10, 20, 30, 40, 50),
                  pal = "purple", pch = 21:26))

expect_silent(leg(
  type = "prop",
  val = c(10, 50, 100),
  pos = "topleft",
  alpha = .5
))
expect_silent(leg(
  type = "prop",
  val = c(10, 50, 100),
  pos = "topright",
  alpha = .5,
  horiz = TRUE
))
expect_silent(leg(
  type = "choro",
  val = c(10, 20, 30, 40, 50),
  pos = "bottomleft",
  alpha = .5
))
expect_silent(leg(
  type = "choro",
  val = c(10, 20, 30, 40, 50),
  pos = "left",
  horiz = TRUE,
  alpha = .5
))

expect_silent(leg(
  type = "typo",
  val = c("A", "B", "C"),
  pos = "top",
  alpha = .5
))
expect_silent(leg(
  type = "symb",
  val = c("A", "B", "C"),
  pos = "right",
  alpha = .5
))
expect_silent(leg(
  type = "prop_line",
  val = c(5, 50, 100),
  pos = "bottom",
  lwd = 20,
  alpha = .5
))

expect_silent(leg(
  type = "grad_line",
  val = c(1, 4, 10, 15),
  pos = "right",
  lwd = c(1, 5, 10),
  alpha = -1.5
))
expect_silent(leg(
  type = "grad_line",
  val = c(1, 4, 10, 15),
  pos = "right",
  lwd = c(1, 5, 10),
  alpha = 1.5
))

plot.new()
plot.window(xlim = c(0, 1),
            ylim = c(0, 1),
            asp = 1)

expect_silent(leg(
  type = "cont",
  val = c(10, 20, 30, 40, 50),
  pos = "bottomleft",
  horiz = TRUE,
  alpha = .5
))
expect_silent(leg(
  type = "cont",
  val = c(10, 20, 30, 40, 50),
  pos = "topright",
  horiz = FALSE,
  alpha = .5
))

expect_silent(leg(
  type = "cont", title = "",
  val = c(10, 20, 30, 40, 50),
  pos = "topright",
  horiz = FALSE,
  alpha = .5
))

expect_error(leg(
  type = "cont",
  val = c(10),
  pos = "topright",
  horiz = FALSE,
  alpha = .5
))


expect_error(leg(
  type = "choro",
  val = c(10),
  pos = "topright",
  horiz = FALSE,
  alpha = .5
))

expect_error(leg(
  type = "choro",
  val = c(10),
  pos = "topright",
  horiz = TRUE,
  alpha = .5
))

expect_error(leg(
  type = "grad_line",
  val = c(10),
  pos = "topright",
  alpha = .5
))


x <- round(rnorm(1000)*1000, 2)
y <- round(rbeta(1000, .6, 7)*1000, 2)
z <- round(rbeta(1000, 5, .6)*1000, 2)
xh <- hist(x, breaks = quantile(x, 0:10 /10), plot = F)
yh <- hist(y, breaks = quantile(y, 0:10 /10), plot = F)
zh <- hist(z, breaks = quantile(z, 0:10 /10), plot = F)
expect_silent({
leg(type = "histo", val = xh, pos = "topleft", frame = T)
leg(type = "histo", val = yh, pos = 'top'    , frame = T)
leg(type = "histo", val = zh, pos = 'bottom' , frame = T)
})

# test 1 modality leg
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
expect_silent({
  leg(type = "prop", val = c(100), pos = "topleft")
  leg(
    type = "prop", val = c(1), symbol = "square", self_adjust = TRUE,
    pos = "topleft"
  )
  leg(type = "choro", val = c(10, 50), pos = "bottomleft")
  leg(type = "typo", val = c("A"), pos = "top")
  leg(type = "symb", val = c("A"), pos = "topright")
  leg(type = "prop_line", val = c(100), pos = "bottom", lwd = 20)
  leg(type = "grad_line", val = c(1, 5), pos = "bottomright", lwd = 10)
  leg(type = "prop", val = c(100), pos = "topleft", horiz = TRUE)
  leg(
    type = "prop", val = c(100), pos = "topleft", symbol = "square",
    horiz = TRUE
  )
  leg(type = "choro", val = c(10, 50), pos = "left", horiz = TRUE)
  leg(
    type = "cont", val = c(10, 20), pos = "bottomleft", horiz = TRUE
  )
  leg(
    type = "cont", val = c(10, 20), pos = "topright",
    horiz = FALSE
  )
  set.seed(46)
  leg(
    type = "histo", val = hist(rnorm(1000), breaks = 5, plot = FALSE),
    pos = "bottomright"
  )
})

