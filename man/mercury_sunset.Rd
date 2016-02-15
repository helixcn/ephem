\name{mercury_sunset}
\alias{mercury_sunset}

\title{
Track of Mercury during sunset for a period
}

\description{
Track of Mercury during sunset for a period
}

\usage{
mercury_sunset(JD, latitude, days = 365,
                 label_interval = 10)
}

\arguments{
  \item{JD}{
Julian Date 
}

  \item{latitude}{
Latitude in decimal degree
} 

  \item{days}{
Number of days to compute
}
  \item{label_interval}{
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
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}
\note{

}

\seealso{
\code{\link{mercury_sunrise}}
}
\examples{
JD <- CAADate_DateToJD(2016, 1, 1, TRUE)
mercury_sunset(JD, latitude = 10,  
      days = 365, label_interval = 20 )

}

\keyword{ mercury }
\keyword{ sunset }
