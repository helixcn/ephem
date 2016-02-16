\name{ephem-package}
\alias{ephem-package}
\alias{ephem}
\docType{package}
\title{
\packageTitle{ephem}
}
\description{
\packageDescription{ephem}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ephem}
\packageIndices{ephem}
creating figures for astronomical almanac
}
\author{
\packageAuthor{ephem}

Maintainer: \packageMaintainer{ephem}
}
\references{

}

\keyword{ package }
\seealso{

}
\examples{

### Latitude (39.0842) of Tianjin, China
venus_sunset(CAADate_DateToJD(2017, 1, 1, TRUE), latitude = 39.0842, days = 365, label_interval = 10 )                                    
venus_sunrise(CAADate_DateToJD(2017, 1, 1, TRUE), latitude = 39.0842, days = 365, label_interval = 10)                                    
mercury_sunrise( CAADate_DateToJD(2017, 1, 1, TRUE) , latitude = 39.0842, days = 365, label_interval = 10)                                
mercury_sunset(CAADate_DateToJD(2017, 1, 1, TRUE), latitude = 39.0842, days = 365, label_interval = 10 )                                  
plot_juptermoons(CAADate_DateToJD(Year = 2017, Month = 1, Day =1, TRUE) , days = 31, increment = 1/24)                                                                   
plot_location_atlas(CAADate_DateToJD(Year = 2017, Month =1, Day =15, TRUE))                                                               
plot_rise_set(CAADate_DateToJD(Year = 2017, Month =1, Day =1, TRUE), days = 365,longitude = -121.468773, latitude = 31.228328,  zone = -8)
plot_transit_table(transit_table(CAADate_DateToJD(Year = 2017, Month =1, Day =1, TRUE), days =365), legend = TRUE)                        

}
