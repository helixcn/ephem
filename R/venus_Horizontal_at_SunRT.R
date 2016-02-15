venus_Horizontal_at_SunRT <-
function(JD, longitude, latitude, zone = 0, type = c("rise", "set")){
     sun_rise_set <- sunRTScalc2(JD, longitude, latitude, zone, type)
     res <- CAAElliptical_Calculate_Venus(sun_rise_set)
     theta0 <- CAASidereal_ApparentGreenwichSiderealTime(sun_rise_set)
     H = theta0 - longitude/15 - res$ApparentGeocentricRA
     CAACoordinateTransformation_Equatorial2Horizontal(H, 
            res$ApparentGeocentricDeclination, latitude)
}
