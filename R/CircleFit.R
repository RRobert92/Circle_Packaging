################################################################################
# R function
#
# The function takes as an input R and r for radius of big and small circle, and
# calculate theoretical maximum number of circles
#
# (c) 2020 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-11-30
################################################################################
Circle_Fit <- function(x, y, type) {
  if (missing(type)) {
    type <- "text"
  }
  if (missing(y)) warning("r for smaller circle radius was not specified")

  if (exists("x") && exists("y")) {
    x <- as.numeric(x)
    y <- as.numeric(y)

    P <- (0.7175 * (x / y)^0.0529)
    n_circle <- round((pi * x^2) / ((pi / P) * y^2), 0)

    if (type == "text") {
      print("Number of fitted circles")
      print(n_circle)
      print("Estimated packaging")
      paste(round((0.7175 * (x / y)^0.0529) * 100, 1), "%", sep = "")
    } else if (type == "data") {
      n_circle <- data.frame(
        "No_circle" = n_circle,
        "Packaging" = round((0.7175 * (x / y)^0.0529) * 100, 1)
      )
      print(n_circle)
    }
  }
}
