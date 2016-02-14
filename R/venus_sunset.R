venus_sunset <-
function(JD, days = 365,lable_interval = 10){
    dat.H <- c()
    dat.Z <- c()
    Year <- c()
    Month <- c()
    Day <- c()
    i = 1
    for(time in JD:(JD+days)){
        rr <- venus_Horizontal_at_SunRT(time, type = "set")
        dat.H[i] <- rr[[1]]
        dat.Z[i] <- rr[[2]]
        rs <- Julian2Date(time) 
        Year[i]  <- rs$Y
        Month[i] <- rs$M
        Day[i]   <- rs$D
        i = i + 1
    }
    dat.table <- data.frame(dat.H, dat.Z, Year, Month, Day)
    plot(dat.Z ~ dat.H, ylim = c(0, 50), xlim = c(0, 150), 
          type = "n", col = "red", xlab = "Azimuth", ylab = "Altitude")
    abline(h = seq(0, 50, by = 5), v = seq(0, 150, by = 5), col = "grey")
    lines(dat.Z ~ dat.H, col = "red")
    
    ind <- seq(1,days, by = lable_interval)
    sel.tab <- dat.table[ind,]
    points(sel.tab$dat.H, sel.tab$dat.Z, pch = 19, col = "red", cex = 0.5)
    text(sel.tab$dat.H, sel.tab$dat.Z+2, paste(sel.tab$M, sel.tab$D, sep = "-"))
    title(paste("Azimuth and altitude of Venus at sunset from ", 
         paste(paste(Year[1], Month[1], Day[1], sep = "-"), "to", 
         paste(Year[i-1], Month[i-1], Day[i-1], sep  = "-"))))
}
