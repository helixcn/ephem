sunRTScalc <-
function(JD, longitude, latitude,  zone, 
                       bGregorianCalendar = TRUE, 
                       type = c("rise", "transit", "set")){
    type <- match.arg(type)
    
    SunDetails1 = CAAElliptical_Calculate_Sun(JD - 1)
    Alpha1 = SunDetails1$ApparentGeocentricRA;
    Delta1 = SunDetails1$ApparentGeocentricDeclination;
    
    SunDetails2 = CAAElliptical_Calculate_Sun(JD)
    Alpha2 = SunDetails2$ApparentGeocentricRA;
    Delta2 = SunDetails2$ApparentGeocentricDeclination;
    
    SunDetails3 = CAAElliptical_Calculate_Sun(JD + 1)
    Alpha3 = SunDetails3$ApparentGeocentricRA;
    Delta3 = SunDetails3$ApparentGeocentricDeclination;
    
    RiseTransitSetTime =CAARiseTransitSet_Calculate(JD, 
    Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3, 
    longitude, latitude, -0.8333);
    
    switch( type, 
    rise = {
        rtsJD = (JD + (RiseTransitSetTime$details.Rise/24.00));
    } ,
    set = {
        rtsJD = (JD + (RiseTransitSetTime$details.Set/24.00));
    } ,
    transit = {
        rtsJD = (JD + (RiseTransitSetTime$details.Transit/24.00));
    }
    )
    lclJD = rtsJD - (zone/24.00); 
    
    names(lclJD) <- paste("Sun", type,  sep = " ")
    class(lclJD) <-"RTS"
    return(lclJD)
}
