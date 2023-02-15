plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)

expect_silent(leg(type = "prop", pos = "topright", val = c(1,5,10), inches = .3))
expect_silent(leg(type = "choro", pos = "bottomright", val = c(10,20,30,40,50),
                        pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "typo", pos = "topleft", val = c("A", "B", "C", "D"),
                        pal = hcl.colors(4, "Dynamic")))
expect_silent(leg(type = "symb", pos = "bottomleft", val = c("A", "B", "C"),
                        pch = 21:23, cex = c(1, 2, 2), pal = hcl.colors(3, "Dynamic")))
expect_silent(leg(type = "grad_line", pos = "top", val =c(1,2,3,4,10,15),
                        lwd = c(0.2,2,4,5,10)))
expect_silent(leg(type = "prop_line", pos = "bottom", lwd = 20, val = c(5,50,100)))

expect_silent(leg(type = "prop", pos = "topright", val = c(2),
                  self_adjust = T, horiz = T, inches = .3))


expect_error(leg(type = "prop_line", pos = "interactive", lwd = 20, val = c(5,50,100)))


expect_error(leg(type = "grad_line", pos = "NA", val =c(1,2,3,4,10,15),
                        lwd = c(0.2,2,4,5,10)))

expect_silent(leg(type = "grad_line", pos = c(737788.682080213, 1628182.17278935),
                        val =c(1,2,3,4,10,15),
                        lwd = c(0.2,2,4,5,10), frame = TRUE))
expect_silent(leg(type = "grad_line", pos = NA,
                        val =c(1,2,3,4,10,15),
                        lwd = c(0.2,2,4,5,10)))

expect_silent(leg(type = "prop_line", pos = NA,
                        lwd = 20,
                        val = c(5,50,100)))
expect_silent(leg(type = "prop_line", pos = c(737788.682080213, 1628182.17278935),
                        lwd = 20,
                        val = c(5,50,100),
                        frame = TRUE))

expect_silent(leg(type = "cont", pos = "top", val = c(10,100)))

expect_silent(leg(type = "cont", pos = "top", val = c(10,100), horiz = TRUE))

expect_silent(leg(type = "choro", pos = c(.5,.5),
                  frame = TRUE, no_data = T, val = c(10,20,30,40,50),
                  pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "typo", pos = c(.5,.5),
                  frame = TRUE, no_data = T, val = c(10,20,30,40,50),
                  pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "symb", pos = c(.1,.8),
                  frame = TRUE, no_data = T, val = c(10,20,30,40,50),
                  pal = "Viridis", pch = 21:26))
expect_silent(leg(type = "choro", pos = c(.5,.5),horiz = T,
                  frame = TRUE, no_data = T, val = c(10,20,30,40,50),
                  pal = hcl.colors(4, "Reds 2")))
expect_silent(leg(type = "prop", pos = c(.1,.8),
                  frame = TRUE, self_adjust = T,
                  val = c(10,20,30,40,50),
                  col = 2))
expect_silent(leg(type = "prop", pos = c(.1,.8),
                  frame = TRUE, self_adjust = T, horiz = TRUE,
                  val = c(10,20,30,40,50),
                  col = 2))

expect_silent(leg(type = "choro", pos = c(.5,.5),
                  frame = TRUE, no_data = T, val = c(10,20,30,40,50),
                  pal = hcl.colors(4, "Reds 2")))
