\name{plot_rise_set}
\alias{plot_rise_set}

\title{
The figure of rise and set time for the major planets
}
\description{
The figure of rise and set time for the major planets
}
\usage{
plot_rise_set(JD, days, longitude, latitude, zone, legend = TRUE, ...)
}

\arguments{
  \item{JD}{
Starting time to compute, in Julian Day
}
  \item{days}{
Number of days
}
  \item{longitude}{
Longitude in decimal degree
} 

  \item{latitude}{
Latitude in decimal degree
} 

  \item{zone}{
Time Zone, Eastern Negative, Western Positive 
}

  \item{legend}{
Whether the legend shall be applied
}
  \item{\dots}{
other arguments to be passed to. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Figure
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jinlong \email{jinlongzhang01@gmail.com}
}
\note{
%%  ~~further notes~~
}


\seealso{
 \code{\link{plot_transit_table}}
}
\examples{

JD = CAADate_DateToJD(Year = 2016, Month =1, Day =1, bGregorianCalendar=TRUE)
plot_rise_set(JD, days = 365,longitude = -121.468773, latitude = 31.228328,  zone = -8) 
plot_rise_set(JD, days = 365,longitude = -116, latitude = 40,  zone = -8) 
plot_rise_set(JD, days = 365,longitude = -127, latitude = 45,  zone = -8) 

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
