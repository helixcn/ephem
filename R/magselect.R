magselect <-
function(magnitude) {
  data(resstar)
  resstar <- resstar
  res <- resstar[resstar[, 3] < magnitude,]
  res <- na.omit(res)
  return(res)
}
