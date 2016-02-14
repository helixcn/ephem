plot_rise_set_table <-
function(rtable, stable, legend = TRUE, ...){
    whichint <- function(x, limit = 1) {
        ind1 <- 1:(length(x) - 1)
        ind2 <- ind1 + 1
        res <- x[ind1] - x[ind2]
        which(abs(res) > limit)
    } 
    
    par(las = 2, xaxs = "i", yaxs = "i") 
    plot(x = rtable[,2], y = (1:length(rtable[,2])), type = "n", 
         axes = FALSE, xlim = c(0, 1), ylim = c(0, nrow(rtable)), 
          xlab = "Hour", ylab = "Date", ...)   
    polygon(x = c(rtable$Sun, rev(stable$Sun)), y = c(1:length(rtable$Sun), rev(1:length(stable$Sun))), col = "skyblue")  
          
    axis(1, at = seq(0, 24, by = 2)/24, labels = paste(seq(0, 24, by = 2), "h", sep = "")) 
    abline(h = c(which(rtable[, 12] ==1),which(rtable[, 12] ==10),which(rtable[, 12] ==20) ), col = "grey") 
    abline(v = seq(0, 24, by = 1)/24, col = "grey") 
    date.lable0 <- rtable[which(rtable[, 12] ==1),][,c(10,11,12)] 
    date.lable <- paste(date.lable0[,2], date.lable0[,3] , sep = "-") 
    axis(2, at = which(rtable[, 12] ==1), labels = date.lable)  

    #col = c("green","orange","firebrick1","burlywood","goldenrod4","lightsteelblue","slateblue","black")
    col = c(1:7, "darkorange")
    lwd <- c(rep(1, 7), 2)
    for(i in 2:9){ 
        if(length(whichint(rtable[,i], limit = .5)) > 0){  
            for(j in 1:length(whichint(rtable[,i], limit = .5)))
            lines(y = (1:whichint(rtable[,i], limit = 0.5)), 
                x = rtable[,i][1:whichint(rtable[,i], limit = 0.5)], col = col[i-1], lwd = lwd[i])
            lines(y = ((whichint(rtable[,i], limit = 0.5)+1):length(rtable[,i])), 
                x = rtable[,i][(whichint(rtable[,i], limit = 0.5)+1):length(rtable[,i])], 
                col = col[i-1], lwd = lwd[i]) 
        } else {
            lines(y = (1:length(rtable[,i])), x = rtable[,i], col = col[i-1], lwd = lwd[i]) 
        }
    }
    
    for(i in 2:10){ 
        if(length(whichint(stable[,i], limit = .5)) > 0){
            for(j in 1:length(whichint(stable[,i], limit = .5)))
            lines(y = (1:whichint(stable[,i], limit = 0.5)), 
                x = stable[,i][1:whichint(stable[,i], limit = 0.5)], col = col[i-1], lty = 2, lwd = lwd[i])
            lines(y = ((whichint(stable[,i], limit = 0.5)+1):length(stable[,i])), 
                x = stable[,i][(whichint(stable[,i], limit = 0.5)+1):length(stable[,i])], 
                col = col[i-1], lty = 2, lwd = lwd[i])
        } else {
            lines(y = (1:length(stable[,i])), x = stable[,i], col = col[i-1], lty = 2, lwd = lwd[i])
        }
    }

    box()
    
    if(legend){
        planet.label <- c("Mercury", "Venus", "Mars", "Jupiter", "Saturn",
         "Uranus", "Neptune", "Sun")
        legend("bottomright", legend = planet.label, lty = 1, col = col)
    }
}
