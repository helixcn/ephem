date2jd <-
function(y, m, d) {
  n = 0
  G = 0
  if (y * 372 + m * 31 + floor(d) >= 588829) {
    G = 1
  }
  if (m <= 2) {
    m = m + 12
    y = y - 1
  }
  if (G) {
    n = floor(y / 100)
    n = 2 - n + floor(n / 4)
  }
  res <- floor(365.25 * (y + 4716)) + floor(30.6001 * (m + 1)) + d + n - 1524.5
  return(res)
}
