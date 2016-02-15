\name{mercury_sunrise}
\alias{mercury_sunrise}

\title{
track of mercury during sunrise for a given period
}
\description{
track of mercury during sunrise for a given period
}
\usage{
mercury_sunrise(JD, latitude, days = 365,
                 label_interval = 10)
}

\arguments{
  \item{JD}{
Julian Date of the starting time
}

  \item{latitude}{
Latitude in decimal degree
} 

  \item{days}{
Number of days
}
  \item{label_interval}{
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
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}
\note{

}

\seealso{
\code{\link{mercury_sunset}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
mercury_sunrise(JD, latitude = 39.9, days = 300,
                 label_interval = 10)
}

\keyword{ mercury }
\keyword{ sunset }
