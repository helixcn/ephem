plot_juptermoons <-
function(JD, days = 30, increment = 1/24, legend = TRUE)
{
    JD.all <- rep(NA, floor(days/increment))
    satellite.1 <- rep(NA, floor(days/increment))
    satellite.2 <- rep(NA, floor(days/increment))
    satellite.3 <- rep(NA, floor(days/increment))
    satellite.4 <- rep(NA, floor(days/increment))
    
    satellite.1.eclipse <- rep(NA, floor(days/increment))
    satellite.2.eclipse <- rep(NA, floor(days/increment))
    satellite.3.eclipse <- rep(NA, floor(days/increment))
    satellite.4.eclipse <- rep(NA, floor(days/increment))
    
    date.Y <- rep(NA, floor(days/increment))
    date.M <- rep(NA, floor(days/increment))
    date.D <- rep(NA, floor(days/increment))
    date.h <- rep(NA, floor(days/increment))
    date.m <- rep(NA, floor(days/increment))
    date.s <- rep(NA, floor(days/increment))
    
    for (i in 1:floor(days/increment)){
        JD <- JD + increment
        JD.all[i] <- JD
        temp <- CAAGalileanMoons_Calculate(JD);
        satellite.1[i] <- temp$ApparentRectangularCoordinates$Satellite1.ApparentRectangularCoordinates.X
        satellite.2[i] <- temp$ApparentRectangularCoordinates$Satellite2.ApparentRectangularCoordinates.X
        satellite.3[i] <- temp$ApparentRectangularCoordinates$Satellite3.ApparentRectangularCoordinates.X
        satellite.4[i] <- temp$ApparentRectangularCoordinates$Satellite4.ApparentRectangularCoordinates.X
        
        satellite.1.eclipse[i] <- temp$Eclipse$Satellite1.bInShadowTransit
        satellite.2.eclipse[i] <- temp$Eclipse$Satellite2.bInShadowTransit
        satellite.3.eclipse[i] <- temp$Eclipse$Satellite3.bInShadowTransit
        satellite.4.eclipse[i] <- temp$Eclipse$Satellite4.bInShadowTransit
        
        res.date <- Julian2Date(JD)
        date.Y[i] <- res.date$Y   
        date.M[i] <- res.date$M   
        date.D[i] <- res.date$D   
        date.h[i] <- res.date$h 
        date.m[i] <- res.date$m     
        date.s[i] <- res.date$s     
    }
    
    res <- data.frame(JD.all,satellite.1,satellite.2,satellite.3,satellite.4,
           satellite.1.eclipse,satellite.2.eclipse,satellite.3.eclipse,satellite.4.eclipse,
           date.Y, date.M, date.D, date.h, date.m, date.s)
    
    par(las = 2, xaxs = "i", yaxs = "i", mar = c(5, 7, 3,2)) 
    plot( JD.all ~ (satellite.4), data = res, xlim = c(-33, 33), axes = FALSE, type = "n", xlab = "", ylab = "", main =  "Galilean Moons of Jupiter")
    
    jddays <- unique(floor(JD.all)) + 0.5
    
    jddays.Y <- c()
    jddays.M <- c()
    jddays.D <- c()
    
    for (i in 1:length(jddays)){
        temp2 <- Julian2Date(jddays[i])
        jddays.Y[i] <- temp2$Y   
        jddays.M[i] <- temp2$M   
        jddays.D[i] <- temp2$D   
    }
    date.label <- paste(jddays.Y,jddays.M,jddays.D, sep = "-")
    
    
    color = rep(c(TRUE, FALSE), length(jddays))

    for(i in 1:(length(jddays)-1)){
        x1 = c(-40, 40, 40, -40)
        y1 = jddays[c(1, 1, 2, 2) + (i - 1)]
        if(color[i]){
            polygon(x = x1, y = y1, col = grey(0.9), border = FALSE)
        }
    }
    
    lines(JD.all ~ (satellite.1), data = res, col = "black")
    lines(JD.all ~ (satellite.2), data = res, col = "green")
    lines(JD.all ~ (satellite.3), data = res, col = "red")
    lines(JD.all ~ (satellite.4), data = res, col = "blue")
    
    axis(2, at = jddays, labels = date.label) 
    axis(1)
    box()
    polygon(x = c(rep(-.5, nrow(res)), rep(.5, nrow(res))), y = c(JD.all, rev(JD.all)), col = "yellow")  
    
    
    if(legend){
        lab <- c("Io", "Europa", "Ganymede", "Callisto")
        legend("bottomright", legend = lab, lty = 1, col = c("black", "green", "red", "blue"))
    }
}
