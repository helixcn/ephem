plot_transit_table <-
function(dat.table, legend = TRUE){
    whichint <- function(x, limit = 1) {
        ind1 <- 1:(length(x) - 1)
        ind2 <- ind1 + 1
        res <- x[ind1] - x[ind2]
        which(abs(res) > limit)
    }
    par(las = 2)
    plot(y = -(1:length(dat.table[,2])), x = -dat.table[,2], type = "n", 
         axes = FALSE, xlim = c(-1, 0), ylim = c(-nrow(dat.table), 0), 
          xlab = "Hour", ylab = "Date")
          
           axis(1, at = -seq(0, 24, by = 4)/24, labels = paste(seq(0, 24, by = 4), "h") )
    abline(h = -c(which(dat.table[, 12] ==1),which(dat.table[, 12] ==10),which(dat.table[, 12] ==20)), col = "grey")
    axis(1, at = -seq(0, 24, by = 2)/24, labels = paste(seq(0, 24, by = 2), "h") )
    abline(v = -seq(0, 24, by = 1)/24, col = "grey")
    date.lable0 <- dat.table[which(dat.table[, 12] ==1),][,c(10,11,12)]
    date.lable <- paste(date.lable0[,2], date.lable0[,3] , sep = "-")
    axis(2, at = -which(dat.table[, 12] ==1), labels = date.lable)
    title(paste("Time of Transits from ",paste(dat.table[1,10:12], collapse = "-"),
    "to", paste(dat.table[nrow(dat.table),][10:12], collapse = "-")))
    polygon(x = -c(dat.table$Sun - 1/48, rev(dat.table$Sun + 1/48)), y = -c(1:length(dat.table$Sun), rev(1:length(dat.table$Sun))), density = 20, col = "grey")  
          
          
    col = 1:9
    for(i in 2:10){
        if(length(whichint(dat.table[,i], limit = 0.5)) > 0){
            lines(y = -(1:whichint(dat.table[,i], limit = 0.5)), 
                x = -dat.table[,i][1:whichint(dat.table[,i], limit = 0.5)], col = col[i-1])
            lines(y = -((whichint(dat.table[,i], limit = 0.5)+1):length(dat.table[,i])), 
                x = -dat.table[,i][(whichint(dat.table[,i], limit = 0.5)+1):length(dat.table[,i])], 
                col = col[i-1])
        } else {
            lines(y = -(1:length(dat.table[,i])), x = -dat.table[,i], col = col[i-1])
        }
    }
    box()
    if(legend){
        planet.label <- c("Mercury", "Venus", "Mars", "Jupiter", "Saturn",
         "Uranus", "Neptune", "Sun")
        legend("bottomright", legend = planet.label, lty = 1, col = 1:9)
    }
}
