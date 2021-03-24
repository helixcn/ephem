add_stars <-
function(xlim = c(0, 24), ylim = c(-30, 30), mag = 3, times = 1, col = "white", ...) {
  ## Subseting stars to be plot
  subres <- magselect(magnitude = mag)
  x1 <- range(xlim)
  x2 <- range(ylim)
  target <- subres[(subres[, 1] >= x1[1]) & (subres[, 1] <= x1[2]) & (subres[, 2] >= x2[1]) & (subres[, 2] <= x2[2]),]

  ## Calculating Size for each star
  if (is.integer(mag)) {
    magint <- 0:mag
  } else {
    magint <- c(0:floor(mag), mag)
  }
  int <- findInterval(target[, 3], magint)
  ddd <- rep(0, length(int))
  pointsize <- (length(magint):1) / length(magint)
  for (i in 1:length(magint)) {
    ind <- (magint[i] == int)
    ddd[ind] <- pointsize[i]
  }
  ## Add stars to a certain background
  points(x = (target[, 1]), y = (target[, 2]), xlim = xlim, ylim = ylim, type = "p", cex = times * ddd, col = col, ...)
}
