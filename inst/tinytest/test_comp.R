expect_silent({
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  box()
  leg_comp(type = "prop", val = c(10,50,100)) |>
    leg_comp(type = "typo", val = c("A", "B", "C")) |>
    leg_draw()
})
