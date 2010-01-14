\name{deg.dist}
\alias{deg.dist}
\title{Haversine Distance Formula}
\description{
  Haversine formula to calculate distances between points on the earth
}
\usage{
deg.dist(lat1, long1, lat2, long2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat1}{latitude of location 1}
  \item{long1}{longitude of location 1}
  \item{lat2}{latitude of location 2}
  \item{long2}{longitude of location 2}
}
\details{
  This function will calculate the shortest distance (portion of a Great Circle)
  in kilometers between two points on the Earth given their latitude and 
  longitude.
}
\value{
  Arc distance between two points on the Earth's surface in kilometers.
}
\references{The formulas for the Haversine distance function were taken from the Dr. Math website at \url{http://mathforum.org/library/drmath/view/55417.html}}
\author{Matthew Vavrek}
\note{
  The distance calculated may be up to 0.2\% inaccurate, as this function 
  treats the Earth as a sphere with a circumference of 40003 km, rather than an
  ellipsoid like it actually is.
}
\seealso{To calculate pairwise distances between a list of points see \code{\link{earth.dist}}, or to calculate an area enclosed by three points on the Earth's surface, see \code{\link{earth.tri}}}
\examples{
##distance between 23 degrees N 54 degrees E and 32 degrees S 67 degrees E
deg.dist(23,54,-32,67)
}
\keyword{data}
