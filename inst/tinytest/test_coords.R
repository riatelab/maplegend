plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
box()

vbox <- vector("list", 10)
vbox[[1]] <- leg(type = "prop", val = c(10,50,100), pos = "topleft", return_bbox = TRUE)
vbox[[2]] <- leg(type = "choro", val = c(10, 20, 30, 40, 50), pos = "bottomleft", return_bbox = TRUE)
vbox[[3]] <- leg(type = "typo", val = c("A", "B", "C"), pos = "top", return_bbox = TRUE)
vbox[[4]] <- leg(type = "symb", val = c("A", "B", "C"), pos = "topright", return_bbox = TRUE)
vbox[[5]] <- leg(type = "prop_line", val = c(5, 50, 100), pos = "bottom", lwd = 20, return_bbox = TRUE)
vbox[[6]] <- leg(type = "grad_line", val = c(1, 4, 10, 15), pos = "bottomright", lwd = c(1,5,10), return_bbox = TRUE)
vbox[[7]] <- leg(type = "cont", val = c(10, 20, 30, 40, 50), pos = "topright", return_bbox = TRUE)
vbox[[8]] <- leg(type = "prop", val = c(10,50,100), pos = "topleft", horiz = TRUE, return_bbox = TRUE)
vbox[[9]] <- leg(type = "choro", val = c(10, 20, 30, 40, 50), pos = "left", horiz = TRUE, return_bbox = TRUE)
vbox[[10]] <- leg(type = "cont", val = c(10, 1000), pos = "bottomleft", horiz = TRUE, return_bbox = TRUE)

# print(dput(vbox))



vbox_ref <- list(list(xleft = -0.102790697674419, ybottom = 0.83543023255814,
                      xright = 0.114848837209302, ytop = 1.04), list(xleft = -0.102790697674419,
                                                                     ybottom = -0.04, xright = 0.0862383720930233, ytop = 0.201270348837209),
                 list(xleft = 0.420014534883721, ybottom = 0.786662790697675,
                      xright = 0.609043604651163, ytop = 1.04), list(xleft = 0.923441860465116,
                                                                     ybottom = 0.80603488372093, xright = 1.10279069767442,
                                                                     ytop = 1.04), list(xleft = 0.420014534883721, ybottom = -0.04,
                                                                                        xright = 0.609043604651163, ytop = 0.213337209302326),
                 list(xleft = 0.913761627906977, ybottom = -0.04, xright = 1.10279069767442,
                      ytop = 0.155220930232558), list(xleft = 0.913761627906977,
                                                      ybottom = 0.759985465116279, xright = 1.10279069767442,
                                                      ytop = 1.04), list(xleft = -0.102790697674419, ybottom = 0.81355523255814,
                                                                         xright = 0.189643990387275, ytop = 1.04), list(xleft = -0.102790697674419,
                                                                                                                        ybottom = 0.420510174418605, xright = 0.413825581395349,
                                                                                                                        ytop = 0.550431686046512), list(xleft = -0.102790697674419,
                                                                                                                                                        ybottom = -0.04, xright = 0.250790697674419, ytop = 0.089921511627907))
#
#
# vbox_ref <-
#   list(
#     list(
#       xleft = -0.927661574618096,
#       ybottom = 0.710035252643949,
#       xright = -0.580070505287897,
#       ytop = 1.04
#     ),
#     list(
#       xleft = -0.927661574618096,
#       ybottom = -0.04,
#       xright = -0.631539365452409,
#       ytop = 0.343666274970623
#     ),
#     list(
#       xleft = 0.374853113983549,
#       ybottom = 0.638119858989424,
#       xright = 0.670975323149236,
#       ytop = 1.04
#     ),
#     list(
#       xleft = 1.64916568742656,
#       ybottom = 0.668672150411281,
#       xright = 1.9276615746181,
#       ytop = 1.04
#     ),
#     list(
#       xleft = 0.374853113983549,
#       ybottom = -0.04,
#       xright = 0.670975323149236,
#       ytop = 0.361880141010576
#     ),
#     list(
#       xleft = 1.63153936545241,
#       ybottom = -0.04,
#       xright = 1.9276615746181,
#       ytop = 0.270223266745006
#     ),
#     list(
#       xleft = 1.63153936545241,
#       ybottom = 0.595229142185664,
#       xright = 1.9276615746181,
#       ytop = 1.04
#     ),
#     list(
#       xleft = -0.927661574618096,
#       ybottom = 0.673901292596945,
#       xright = -0.455499258786263,
#       ytop = 1.04
#     ),
#     list(
#       xleft = -0.927661574618096,
#       ybottom = 0.37264982373678,
#       xright = -0.113325499412456,
#       ytop = 0.581521739130435
#     ),
#     list(
#       xleft = -0.927661574618096,
#       ybottom = -0.04,
#       xright = -0.370669800235018,
#       ytop = 0.168871915393654
#     )
#   )

expect_equal(vbox, vbox_ref)
