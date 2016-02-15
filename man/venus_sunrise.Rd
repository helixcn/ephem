\name{venus_sunrise}
\alias{venus_sunrise}

\title{
Track of venus during sunrise for a given period
}
\description{
Track of venus during sunrise for a given period
}
\usage{
venus_sunrise(JD, longitude, latitude, days = 365, label_interval = 10)
}

\arguments{
  \item{JD}{
The starting time for computation in Julian Date
}

\item{longitude}{
Longitude in decimal degree
} 

\item{latitude}{
Latitude in decimal degree
} 

  \item{days}{
Days the computation will last.
}
  \item{label_interval}{
Intervals the labels to be added. 
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
\note{

}

\seealso{
   \code{\link{venus_sunset}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
venus_sunrise(JD, longitude = -116.5, latitude = 39.9, days = 300,
                 label_interval = 10)
}

\keyword{ venus }
