background <-
function(xlim = c(24, 0), ylim = c(-50, 50), mag = 3, cex = 1, lab = FALSE, grid = FALSE,...)
{
   data(constellation)
   par(xaxs = "i", yaxs = "i")
   plot(constellation$RA, constellation$DEC, cex =  log(constellation$MAG), col = "grey", xlab = "Right Ascension (h)", ylab = expression(paste("Declination", degree)), xlim = xlim, ylim = ylim, axes = FALSE, type = "n")
   
   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "skyblue")
   
   points(constellation$RA, constellation$DEC, cex =  0.2*log(constellation$MAG), pch = 19, type = "l", col = "white")
   
   points(constellation$RA, constellation$DEC, cex =  0.2*log(constellation$MAG), pch = 19, col = "white")
   
   ### abline(h = c(-60, -30, 0, 30, 60), v = seq(0,24,by = 3))
   abline(h = 0, v = 12)
   
   ### par(xaxs = "i", yaxs = "i")
   ### plot.default(0, 0, xlim = xlim, ylim = ylim, axes = FALSE, ...)
   axis(1, at = c(24,  21,  18,  15, 12, 9, 6, 3, 0), labels = c("24h", "21h" ,"18h","15h", "12h","9h" ,"6h","3h" ,"0h"))
   axis(2, at = seq(-60, 60, by = 15), labels = c("-60", "-45", "-30", "-15", "0", "15", "30", "45", "60" ))
   abline(v = 12)
   abline(h = 0)
   
   box()
}
