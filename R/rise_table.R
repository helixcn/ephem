rise_table <-
function(start.JD, days = 30, longitude = -116.391229, latitude =  39.905384,  zone = -8.0) { 

    JD.dat <- seq(start.JD, start.JD+days+1, by = 1)  
    dat.table <- data.frame(JD.dat)                   
    
    for(i in 1:days){ 
    
    temp <- planetRTScalc2(planet = "mercury", JD.dat[i], longitude = longitude, latitude =  latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise") - dat.table[i,1]   
    dat.table[i,2] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
               
    temp <- planetRTScalc2(planet = "venus", JD.dat[i], longitude = longitude, latitude = latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1]
    dat.table[i,3] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
    temp <- planetRTScalc2(planet = "mars",  JD.dat[i],longitude = longitude, latitude =  latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1]
    dat.table[i,4] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
    temp <- planetRTScalc2(planet = "jupiter", JD.dat[i], longitude = longitude, latitude = latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1]
    dat.table[i,5] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
    temp <- planetRTScalc2(planet = "saturn",  JD.dat[i], longitude = longitude, latitude = latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1]
    dat.table[i,6] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
    temp <- planetRTScalc2(planet = "uranus",  JD.dat[i], longitude = longitude, latitude = latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1]
    dat.table[i,7] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
    temp <- planetRTScalc2(planet = "neptune", JD.dat[i], longitude = longitude, latitude = latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1]
    dat.table[i,8] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
    temp <- sunRTScalc2 (JD.dat[i], longitude = longitude, latitude = latitude,  zone = zone, bGregorianCalendar = TRUE, type = "rise")  - dat.table[i,1] 
    dat.table[i,9] <- ifelse(ifelse(temp < 0, temp + 1, temp)>1, ifelse(temp < 0, temp + 1, temp) - 1, ifelse(temp < 0, temp + 1, temp))
    
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
