\name{venus_sunrise}
\alias{venus_sunrise}

\title{
Track of venus during sunrise for a given period
}
\description{
Track of venus during sunrise for a given period
}
\usage{
venus_sunrise(JD, days = 365, lable_interval = 10)
}

\arguments{
  \item{JD}{
The starting time for computation in Julian Date
}
  \item{days}{
Days the computation will last.
}
  \item{lable_interval}{
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
Jinlong Zhang
}
\note{

}

\seealso{
   \code{\link{venus_sunset}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
venus_sunrise(JD, lable_interval = 20)
}

\keyword{ venus }
