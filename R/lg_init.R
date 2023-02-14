leg_test_pos <- function(pos) {
  authorized_pos <-
    c(
      "bottomleft",
      "left",
      "topleft",
      "top",
      "bottom",
      "bottomright",
      "right",
      "topright",
      "bottomleft1",
      "bottomright1",
      "bottom1",
      "bottomleft2",
      "bottomright2",
      "bottom2",
      "topright1",
      "topleft1",
      "top1",
      "topright2",
      "topleft2",
      "top2",
      "interactive"
    )

  # stop if the position is not valid
  if (length(pos) == 1) {
    if (!pos %in% authorized_pos) {
      stop("This legend position is not allowed", call. = FALSE)
    }
  }
}

#' @importFrom grDevices dev.list
leg_test_cur_plot <- function() {
  if (is.null(dev.list())) {
    stop("You can only plot legends on an existing plot.", call. = FALSE)
  }

}

leg_test_input <- function(pos) {
  leg_test_pos(pos)
  leg_test_cur_plot()
}
