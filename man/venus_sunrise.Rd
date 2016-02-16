\name{venus_sunrise}
\alias{venus_sunrise}

\title{
Track of venus during sunrise for a given period
}
\description{
Track of venus during sunrise for a given period
}
\usage{
venus_sunrise(JD, latitude, days = 365, label_interval = 10)
}

\arguments{
  \item{JD}{
The starting time for computation in Julian Date
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
The azimuth is measured westward from the south. 
}

\seealso{
   \code{\link{venus_sunset}}
}
\examples{
venus_sunrise(CAADate_DateToJD(2016, 1, 1, TRUE), latitude = 39.9, days = 300, label_interval = 10)
}

\keyword{ venus }
