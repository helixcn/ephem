\name{transit_table}
\alias{transit_table}

\title{
compute the time of transit for major planets
}
\description{
compute the time of transit for major planets
}
\usage{
transit_table(start.JD, days = 30)
}

\arguments{
  \item{start.JD}{
starting date in Julian Date
}
  \item{days}{
Number of days the computation will last
}
}
\details{

}
\value{
data frame
}
\references{

}
\author{
Jinlong Zhang
}
\note{

}

\seealso{

}
\examples{
JD = CAADate_DateToJD(Year = 2016, Month =1, Day =1, bGregorianCalendar=TRUE)
transit_table(JD, days =365)
}

\keyword{ transit }
