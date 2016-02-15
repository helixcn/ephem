\name{plot_rise_set}
\alias{plot_rise_set}

\title{
The figure of rise and set time for the major planets
}
\description{
The figure of rise and set time for the major planets
}
\usage{
plot_rise_set(JD, days, longitude, latitude, zone, legend = TRUE, ...)
}

\arguments{
  \item{JD}{
Starting time to compute, in Julian Day
}
  \item{days}{
Number of days
}
  \item{longitude}{
longitude of the location
}
  \item{latitude}{
latitude of the location
}
  \item{zone}{
time zone of the location
}
  \item{legend}{
Whether the legend shall be applied
}
  \item{\dots}{
other arguments to be passed to. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Figure
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jinlong
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

JD = CAADate_DateToJD(Year = 2016, Month =1, Day =1, bGregorianCalendar=TRUE)
sss <- plot_rise_set(JD, days = 365,longitude = -121.468773, latitude =  31.228328,  zone = -8) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
