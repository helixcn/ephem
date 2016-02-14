\name{mercury_sunset}
\alias{mercury_sunset}

\title{
Track of Mercury during sunset for a period
}

\description{
Track of Mercury during sunset for a period
}

\usage{
mercury_sunset(JD, days = 365, lable_interval = 10)
}

\arguments{
  \item{JD}{
Julian Date
}
  \item{days}{
Number of days to compute
}
  \item{lable_interval}{
Intervals in day for labels
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
\code{\link{mercury_sunrise}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
mercury_sunset(JD)
}

\keyword{ mercury }
\keyword{ sunset }
