mercury_sunrise <-
function(JD, latitude, days = 365, label_interval = 10) {
  dat.H <- c()
  dat.Z <- c()
  Year <- c()
  Month <- c()
  Day <- c()
  i = 1
  for (ttt in JD:(JD + days)) {
    rr <- mercury_Horizontal_at_SunRT(JD = ttt, longitude = 0, latitude = latitude,
                                          zone = 0, type = "rise")
    dat.H[i] <- rr[[1]]
    dat.Z[i] <- rr[[2]]
    rs <- Julian2Date(ttt)
    Year[i] <- rs$Y
    Month[i] <- rs$M
    Day[i] <- rs$D
    i = i + 1
  }
  dat.table <- data.frame(dat.H, dat.Z, Year, Month, Day)
  plot(dat.Z ~ dat.H, ylim = c(0, 22), xlim = c(220, 330), type = "n",
         col = "red", xlab = "Azimuth", ylab = "Altitude")
  abline(h = seq(0, 25, by = 5), v = seq(220, 330, by = 5), col = "grey")
  lines(dat.Z ~ dat.H, col = "red")
  ind <- seq(1, days, by = label_interval)
  sel.tab <- dat.table[ind,]
  points(sel.tab$dat.H, sel.tab$dat.Z, pch = 19, col = "red", cex = 0.5)
  text(sel.tab$dat.H, sel.tab$dat.Z + 2, paste(sel.tab$M, sel.tab$D, sep = "-"))
  title(paste("Azimuth and altitude of Mercury at sunrise from ",
         paste(paste(Year[1], Month[1], Day[1], sep = "-"), "to",
         paste(Year[i - 1], Month[i - 1], Day[i - 1], sep = "-")), "\n(Lat:", latitude, ")"))
  return(invisible(dat.table))
}
