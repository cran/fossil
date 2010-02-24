\name{earth.bear}
\alias{earth.bear}
\title{Bearings Between Geographic Locations}
\description{
  Calculate the bearing in degrees clockwise from True North between any two 
  points on the globe.
}
\usage{
earth.bear(long1, lat1, long2, lat2)
}
\arguments{
  \item{long1}{Longitude of site 1}
  \item{lat1}{Latitude of site 1}
  \item{long2}{Longitude of site 2}
  \item{lat2}{Latitude of site 2}
}
\details{
  Calculate the bearing in degrees clockwise from True North between any two 
  points on the globe. Primarily designed to be used with other included 
  geographic tools.
}
\value{
  Returns a value in degrees from True North between two geographic points.
}
\references{Haversine formula from Math Forums: Ask Dr. Math at http://mathforum.org/dr.math/}
\author{Matthew Vavrek}
\seealso{\code{\link{earth.poly}}}
\examples{
earth.bear(-100, 30, 20, -40)
}
\keyword{manip}
