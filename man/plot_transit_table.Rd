\name{plot_transit_table}
\alias{plot_transit_table}

\title{
Figure for the time of Transets of major planets 
}
\description{
Figure for the time of Transets of major planets 
}
\usage{
plot_transit_table(dat.table, legend = TRUE)
}

\arguments{
  \item{dat.table}{
data frame
}
  \item{legend}{
Logical, whether the legend will be added to the figure
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
Jinlong Zhang
}
\note{

}

\seealso{
transit_table
}
\examples{
JD = CAADate_DateToJD(Year = 2016, Month =1, Day =1, bGregorianCalendar=TRUE)
plot_transit_table(transit_table(JD, days =365), legend = TRUE)
}

\keyword{ transit }
\keyword{ planet }
