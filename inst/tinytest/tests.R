## internal function
graphics.off()
expect_error(maplegend:::leg_test_input("top"))
expect_equal(maplegend:::get_val_rnd(val = c(5.61, 6.23, 1012.86), val_rnd = 1, val_dec = ",", val_big = " "), c("5,6", "6,2","1 012,9"))
expect_warning(maplegend:::get_val_rnd(val = c(5.61, 6.23, 1012.86), val_rnd = 1, val_dec = ",,"))
expect_warning(maplegend:::get_val_rnd(val = c(5.61, 6.23, 1012.86), val_rnd = 1, val_dec = ""))

plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
expect_error(maplegend:::leg_test_input("pop"))
expect_silent(maplegend:::leg_test_input("top"))
expect_null(leg(type = "prop", val = 1:5, pos = NA))
expect_equal(maplegend:::get_pal("red", 2), c("#FF0000FF", "#FF0000FF"))
expect_equal(maplegend:::get_alpha(-1), "00")
expect_equal(maplegend:::get_alpha(10), "FF")
expect_equal(maplegend:::get_title_dim("", title_cex = 2), list(w = 0, h = 0))


## prop
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
expect_silent(leg(type = "prop", pos = "topright", val = c(1, 5, 10), border = "white"))
expect_silent(leg(type = "prop", pos = "top", val = c(1, 5, 10), symbol = "square", col = "blue"))
expect_silent(leg(type = "prop", pos = "right", symbol = 'square', val = c(1, 5, 10), horiz = TRUE))
expect_silent(leg(type = "prop", pos = "left", symbol = 'square', val = c(1, 5, 10), self_adjust = TRUE))
expect_silent(leg(type = "prop", pos = c(.25,.75), val = c(1, 5, 10), self_adjust = TRUE, horiz = TRUE))
expect_silent(leg(type = "prop", pos = "topleft", val = c(1, 5, 10), val_max = 50))
expect_silent(leg(type = "prop", pos = "bottomleft", val = c(1, 5, 10), alpha = .5, frame = TRUE))
expect_silent(leg(type = "prop", pos = "bottomright", val = c(1, 5, 10), horiz = TRUE, frame = TRUE))
expect_silent(leg(type = "prop", pos = "bottom", val = 10))
expect_silent(leg(type = "prop", pos = c(.5,.5), val = c(1, 5, 10)))

## choro
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10, 20, 30, 40, 50)
expect_silent(leg(type = "choro", pos = "topleft", val = bks))
expect_silent(leg(type = "choro", pos = "topright", val = bks, horiz = TRUE))
expect_silent(leg(type = "choro", pos = "left", val = bks, frame = TRUE))
expect_silent(leg(type = "choro", pos = "right", val = bks, horiz = TRUE, frame = TRUE))
expect_silent(leg(type = "choro", pos = "bottomleft", val = bks, no_data = TRUE))
expect_silent(leg(type = "choro", pos = "bottomright", val = bks, horiz = TRUE, no_data = TRUE))
expect_silent(leg(type = "choro", pos = "top", val = bks, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "choro", pos = "bottom", val = bks, horiz = TRUE, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "choro", val = c(10, 50), pos = c(.5,.5)))
expect_silent(leg(type = "choro", val = c(10, 50), pos = c(.25,.75), horiz = TRUE))
expect_error(leg(type = "choro", val = 10, pos = "topright"))
expect_error(leg(type = "choro", val = 10, pos = "topright", horiz = TRUE))

## cont
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10, 20, 30, 40, 50)
expect_silent(leg(type = "cont", pos = "topleft", val = bks))
expect_silent(leg(type = "cont", pos = "topright", val = bks, horiz = TRUE))
expect_silent(leg(type = "cont", pos = "left", val = bks, frame = TRUE))
expect_silent(leg(type = "cont", pos = "right", val = bks, horiz = TRUE, frame = TRUE))
expect_silent(leg(type = "cont", pos = "bottomleft", val = bks, no_data = TRUE, col_na = "blue"))
expect_silent(leg(type = "cont", pos = "bottomright", val = bks, horiz = TRUE, no_data = TRUE, col_na = "blue"))
expect_silent(leg(type = "cont", pos = "top", val = bks, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "cont", pos = "bottom", val = bks, horiz = TRUE, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "cont", val = c(10, 50), pos = c(.5,.5)))
expect_silent(leg(type = "cont", val = c(10, 50), pos = c(.25,.75), horiz = TRUE))
expect_error(leg(type = "cont", val = 10, pos = "topright"))
expect_error(leg(type = "cont", val = 10, pos = "topright", horiz = TRUE))

graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10,  50)
expect_silent(leg(type = "cont", pos = "topleft", val = bks))
expect_silent(leg(type = "cont", pos = "topright", val = bks, horiz = TRUE))
expect_silent(leg(type = "cont", pos = "left", val = bks, pal = c("red", "blue")))
expect_silent(leg(type = "cont", pos = "right", val = bks, horiz = TRUE, pal = c("red", "blue")))

## typo
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
mod <- c("mod 1", "mod 2 ", "mod 3")
expect_silent(leg(type = "typo", pos = "topleft", val = mod))
expect_silent(leg(type = "typo", pos = "left", val = mod, frame = TRUE))
expect_silent(leg(type = "typo", pos = "bottomleft", val = mod, no_data = TRUE))
expect_silent(leg(type = "typo", pos = "top", val = mod, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "typo", val = mod, pos = c(.5,.5)))
expect_silent(leg(type = "typo", val = 10, pos = "topright"))

## symb
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
mod <- c("mod 1", "mod 2 ", "mod 3")
expect_silent(leg(type = "symb", pos = "topleft", val = mod))
expect_silent(leg(type = "symb", pos = "left", val = mod, frame = TRUE))
expect_silent(leg(type = "symb", pos = "bottomleft", val = mod, no_data = TRUE))
expect_silent(leg(type = "symb", pos = "top", val = mod, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "symb", val = mod, pos = c(.5,.5)))
expect_silent(leg(type = "symb", val = 10, pos = "topright"))
expect_silent(leg(type = "symb", pos = "right", val = mod, pch = 21:23))
expect_silent(leg(type = "symb", pos = "bottomright", val = mod, pch = 22, cex = 2:5, lwd = 2:3))
expect_silent(leg(type = "symb", pos = "bottom", val = mod, no_data = TRUE))

## prop_line
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
expect_silent(leg(type = "prop_line", pos = "topright", val = c(1, 5, 10), lwd = 10))
expect_silent(leg(type = "prop_line", pos = "top", val = c(1, 5, 10), col = "blue", lwd = 10, frame = TRUE))
expect_silent(leg(type = "prop_line", pos = "topleft", val = c( 10), lwd = 10))
expect_silent(leg(type = "prop_line", pos = c(.5,.5), val = c(1, 5, 10), lwd = 5))

## grad_line
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10, 20, 30, 40, 50)
expect_silent(leg(type = "grad_line", pos = "topleft", val = bks, lwd = c(1,2,4,8)))
expect_silent(leg(type = "grad_line", pos = "left", val = bks, frame = TRUE, lwd = c(1,2,4,8)))
expect_silent(leg(type = "grad_line", pos = "bottomleft", val = bks, lwd = c(1,2,4,8)))
expect_silent(leg(type = "grad_line", pos = "top", val = bks, alpha = .5, lwd = c(1,2,4,8)))
expect_silent(leg(type = "grad_line", val = c(.10, 50), pos = c(.5,.5)))
expect_error(leg(type = "grad_line", val = 10, pos = "topright"), class = "error")

## histo
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
set.seed(46)
x <- round(rnorm(1000) * 1000, 2)
xh <- hist(x, breaks = quantile(x, 0:10 /10), plot = FALSE)
expect_silent(leg(type = "histo", val = xh, pos = "topleft", frame = TRUE))
expect_silent(leg(type = "histo", val = xh, pos = 'bottomright'    , frame = FALSE))

## choro_point
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10, 20, 30, 40, 50)
expect_silent(leg(type = "choro_point", pos = "topleft", val = bks))
expect_silent(leg(type = "choro_point", pos = "topright", val = bks, horiz = TRUE))
expect_silent(leg(type = "choro_point", pos = "left", val = bks, frame = TRUE))
expect_silent(leg(type = "choro_point", pos = "right", val = bks, horiz = TRUE, frame = TRUE))
expect_silent(leg(type = "choro_point", pos = "bottomleft", val = bks, no_data = TRUE))
expect_silent(leg(type = "choro_point", pos = "bottomright", val = bks, horiz = TRUE, no_data = TRUE))
expect_silent(leg(type = "choro_point", pos = "top", val = bks, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "choro_point", pos = "bottom", val = bks, horiz = TRUE, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "choro_point", val = c(10, 50), pos = c(.5,.5)))
expect_silent(leg(type = "choro_point", val = c(10, 50), pos = c(.25,.75), horiz = TRUE))
expect_error(leg(type = "choro_point", val = 10, pos = "topright"), class = "error")
expect_error(leg(type = "choro_point", val = 10, pos = "topright", horiz = TRUE), class = "error")
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
expect_silent(leg(type = "choro_point", pos = "topleft", val = bks, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "topright", val = bks, horiz = TRUE, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "left", val = bks, frame = TRUE, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "right", val = bks, horiz = TRUE, frame = TRUE, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "bottomleft", val = bks, no_data = TRUE, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "bottomright", val = bks, horiz = TRUE, no_data = TRUE, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "top", val = bks, pal = hcl.colors(4, "Reds 2"), alpha = .5, symbol = "square"))
expect_silent(leg(type = "choro_point", pos = "bottom", val = bks, horiz = TRUE, pal = hcl.colors(4, "Reds 2"), alpha = .5, symbol = "square"))
expect_silent(leg(type = "choro_point", val = c(10, 50), pos = c(.5,.5), symbol = "square"))
expect_silent(leg(type = "choro_point", val = c(10, 50), pos = c(.25,.75), horiz = TRUE, symbol = "square"))

## choro_line
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10, 20, 30, 40, 50)
expect_silent(leg(type = "choro_line", pos = "topleft", val = bks))
expect_silent(leg(type = "choro_line", pos = "left", val = bks, frame = TRUE))
expect_silent(leg(type = "choro_line", pos = "bottomleft", val = bks, no_data = TRUE))
expect_silent(leg(type = "choro_line", pos = "top", val = bks, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "choro_line", val = c(10, 50), pos = c(.5,.5)))
expect_error(leg(type = "choro_line", val = 10, pos = "topright"), class = "error")
expect_error(leg(type = "choro_line", val = 10, pos = "topright", horiz = TRUE), class = "error")


## choro_symb
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
bks <- c(10, 20, 30, 40, 50)
expect_silent(leg(type = "choro_symb", pos = "topleft", val = bks))
expect_silent(leg(type = "choro_symb", pos = "left", val = bks, frame = TRUE))
expect_silent(leg(type = "choro_symb", pos = "bottomleft", val = bks, no_data = TRUE))
expect_silent(leg(type = "choro_symb", pos = "top", val = bks, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "choro_symb", val = c(10, 50), pos = c(.5,.5)))
expect_error(leg(type = "choro_symb", val = 10, pos = "topright"), class = "error")
expect_error(leg(type = "choro_symb", val = 10, pos = "topright", horiz = TRUE), class = "error")


## typo_line
graphics.off()
plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1),
     type = "n", xlab = NA, ylab = NA, axes = FALSE)
mod <- c("mod 1", "mod 2 ", "mod 3")
expect_silent(leg(type = "typo_line", pos = "topleft", val = mod))
expect_silent(leg(type = "typo_line", pos = "left", val = mod, frame = TRUE))
expect_silent(leg(type = "typo_line", pos = "bottomleft", val = mod, no_data = TRUE))
expect_silent(leg(type = "typo_line", pos = "top", val = mod, pal = hcl.colors(4, "Reds 2"), alpha = .5))
expect_silent(leg(type = "typo_line", val = mod, pos = c(.5,.5)))
expect_silent(leg(type = "typo_line", val = 10, pos = "topright"))



