expect_silent({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "typo", val = c("A", "B", "C")) |>
    leg_draw()
})

expect_silent({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "typo", val = c("A", "B", "C")) |>
    leg_draw(pos = NA)
}
)

expect_silent({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "typo", val = c("A", "B", "C")) |>
    leg_draw(pos = "topright")
}
)



expect_silent({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "histo", val = hist(rnorm(150), plot = FALSE)) |>
    leg_draw(pos = "right")
}
)



expect_silent({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "typo", val = c("A", "B", "C")) |>
    leg_draw(pos = c(.5, .5))
}
)




expect_error({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10, 50, 100)) |>
    leg_comp(type = "typo", val = c("A", "B", "C")) |>
    leg_draw(pos = "interactive")
}
)
