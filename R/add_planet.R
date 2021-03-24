add_planet <-
function(time, labels = TRUE, ...) {
  labs <- c("Sun", "Moon", "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto")
  position <- planets(time)
  points(position[, 1], position[, 2], ...)
  if (labels) {
    text(position[, 1], position[, 2] + 5, labs, cex = 1.2)
  }
  return(position)
}
