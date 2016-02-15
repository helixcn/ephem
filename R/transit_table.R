transit_table <-
function(start.JD, days = 30) { 
    JD.dat <- seq(start.JD, start.JD+days+1, by = 1)
    transit <- rep(NA, length(JD.dat))
    dat.table <- data.frame(JD.dat)
    
    for(i in 1:days){
    dat.table[i,2] <- planetRTScalc2(planet = "mercury", JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0, type = "transit") - dat.table[i,1]
               
    dat.table[i,3] <- planetRTScalc2(planet = "venus", JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
               
    dat.table[i,4] <- planetRTScalc2(planet = "mars",  JD.dat[i],longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
    
    dat.table[i,5] <- planetRTScalc2(planet = "jupiter", JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
    
    dat.table[i,6] <- planetRTScalc2(planet = "saturn",  JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
    
    dat.table[i,7] <- planetRTScalc2(planet = "uranus",  JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
    
    dat.table[i,8] <- planetRTScalc2(planet = "neptune", JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
    
    dat.table[i,8] <- planetRTScalc2(planet = "neptune", JD.dat[i], longitude = 0, 
               latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
               
    dat.table[i,9] <- sunRTScalc2 (JD.dat[i], longitude = 0, 
        latitude = 0, zone = 0,  type = "transit")  - dat.table[i,1]
    
    res.date <- Julian2Date(dat.table[i,1])
        dat.table[i, 10] <- res.date$Y
        dat.table[i, 11] <- res.date$M
        dat.table[i, 12] <- res.date$D
    }
    dat.table <- na.omit(dat.table)
    colnames(dat.table) <- c("JD", "Mercury", "Venus", "Mars", "Jupiter", "Saturn",
     "Uranus", "Neptune", "Sun", "Year", "Month", "Day")
    return(dat.table)
}
