mercury_Horizontal_at_SunRT <-
function(JD, longitude = 0, zone = 0, latitude = 40,  
          bGregorianCalendar = TRUE, type = "set")
{
     sun_rise_set <- sunRTScalc(JD, longitude, latitude,  zone,  bGregorianCalendar, type)
     res <- CAAElliptical_Calculate_Mercury(sun_rise_set)
     theta0 <- CAASidereal_ApparentGreenwichSiderealTime(sun_rise_set)
     H = theta0 - longitude/15 - res$ApparentGeocentricRA
     CAACoordinateTransformation_Equatorial2Horizontal(H, res$ApparentGeocentricDeclination, latitude)
}
