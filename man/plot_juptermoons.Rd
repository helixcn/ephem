\name{plot_juptermoons}
\alias{plot_juptermoons}

\title{
Track of Jupiter's Galilean Moons
}
\description{
Track of Jupiter's Galilean Moons
}
\usage{
plot_juptermoons(JD, days = 30, increment = 1/1000, legend = TRUE)
}

\arguments{
  \item{JD}{
Julian Date
}
  \item{days}{
Number of days to compute
}
  \item{increment}{
Increment in days
}
  \item{legend}{
Logical, whether the legend will be added to the figure. 
}
}
\details{

}
\value{
figure
}
\references{

}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}
\note{

}

\seealso{

}
\examples{
JD = CAADate_DateToJD(Year = 2016, Month =1, Day =1, bGregorianCalendar=TRUE)
plot_juptermoons(JD , days = 31, increment = 1/24)
}

\keyword{ Jupitor }
\keyword{ moon }
