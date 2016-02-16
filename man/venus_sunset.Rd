\name{venus_sunset}
\alias{venus_sunset}

\title{
Track of venus during sunset for a given period
}
\description{
Track of venus during sunset for a given period
}
\usage{
venus_sunset(JD, latitude, days = 365, label_interval = 10)
}
\arguments{
  \item{JD}{
The starting time of the track in Julian Date 
}

  \item{latitude}{
Latitude in decimal degree
} 

  \item{days}{
Number of days to compute
}
  \item{label_interval}{
Intervals for the labels in days
}
}
\details{

}
\value{
Figure and the corresponding data frame. 
}
\references{

}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\note{
The azimuth is measured westward from the south. 
}

\seealso{
\code{\link{venus_sunrise}}
}
\examples{
venus_sunset(CAADate_DateToJD(2012, 1, 1, TRUE), latitude = 45, days = 365, label_interval = 20 )
}

\keyword{ Venus }
\keyword{ sunset }
