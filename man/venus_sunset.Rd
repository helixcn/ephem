\name{venus_sunset}
\alias{venus_sunset}

\title{
Track of venus during sunset for a given period
}
\description{
Track of venus during sunset for a given period
}
\usage{
venus_sunset(JD, days = 365, lable_interval = 10)
}
\arguments{
  \item{JD}{
The starting time of the track in Julian Date 
}
  \item{days}{
Number of days 
}
  \item{lable_interval}{
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
Jinlong Zhang
}

\seealso{
\code{\link{venus_sunrise}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
venus_sunset(JD, lable_interval = 20 )
}

\keyword{ Venus }
\keyword{ sunset }
