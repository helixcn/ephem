planetRTScalc2 <-
function(planet = c("mercury", "venus", "mars", 
                 "jupiter", "saturn", "uranus", "neptune", "pluto"), 
                 JD, longitude, latitude, zone, 
                 type = c("rise", "transit", "set")){
    planet <- match.arg(planet)             
    type <- match.arg(type)
    switch(planet, mercury = {
        Details1 = CAAElliptical_Calculate_Mercury(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Mercury(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Mercury(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
     venus = {
        Details1 = CAAElliptical_Calculate_Venus(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Venus(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Venus(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
     mars = {
        Details1 = CAAElliptical_Calculate_Mars(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Mars(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Mars(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
     jupiter = {
        Details1 = CAAElliptical_Calculate_Jupiter(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Jupiter(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Jupiter(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
     saturn = {
        Details1 = CAAElliptical_Calculate_Saturn(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Saturn(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Saturn(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
     uranus = {
        Details1 = CAAElliptical_Calculate_Uranus(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Uranus(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Uranus(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
    neptune = {
        Details1 = CAAElliptical_Calculate_Neptune(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Neptune(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Neptune(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    },
     pluto = {
        Details1 = CAAElliptical_Calculate_Pluto(JD - 1)
        Alpha1 = Details1$ApparentGeocentricRA;
        Delta1 = Details1$ApparentGeocentricDeclination;
        
        Details2 = CAAElliptical_Calculate_Pluto(JD)
        Alpha2 = Details2$ApparentGeocentricRA;
        Delta2 = Details2$ApparentGeocentricDeclination;
        
        Details3 = CAAElliptical_Calculate_Pluto(JD + 1)
        Alpha3 = Details3$ApparentGeocentricRA;
        Delta3 = Details3$ApparentGeocentricDeclination;
    }
    )
    RiseTransitSetTime =CAARiseTransitSet_Calculate(JD, 
        Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3, 
        longitude, latitude, -0.5667);
    
    switch(type, 
    rise = {
        rtsJD = (JD + (RiseTransitSetTime$details.Rise/24.00));
    },
    set = {
        rtsJD = (JD + (RiseTransitSetTime$details.Set/24.00));
    } ,
    transit = {
        rtsJD = (JD + (RiseTransitSetTime$details.Transit/24.00));
    }
    )
    lclJD = rtsJD - (zone/24.00); 
    names(lclJD) <- paste(type, "of", planet,  collapse = " ")
    class(lclJD) <-"RTS"
    return(lclJD)
}
