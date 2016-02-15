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
Number of days 
}
  \item{label_interval}{
Intervals in days
}
}
\details{

}
\value{
Figure
}
\references{

}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
\code{\link{venus_sunrise}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
venus_sunset(JD, latitude = 45, 
             days = 365, label_interval = 20 )
}

\keyword{ Venus }
\keyword{ sunset }
