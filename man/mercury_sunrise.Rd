\name{mercury_sunrise}
\alias{mercury_sunrise}

\title{
track of mercury during sunrise for a given period
}
\description{
track of mercury during sunrise for a given period
}
\usage{
mercury_sunrise(JD, days = 365, lable_interval = 10)
}

\arguments{
  \item{JD}{
Julian Date of the starting time
}
  \item{days}{
Number of days
}
  \item{lable_interval}{
days between each label on the apparent track
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
\code{\link{mercury_sunset}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
mercury_sunrise(JD)
}

\keyword{ mercury }
\keyword{ sunset }
