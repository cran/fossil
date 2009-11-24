\name{earth.bear}
\alias{earth.bear}
\title{Bearings Between Geographic Locations}
\description{
  Calculate the bearing in degrees clockwise from True North between any two 
  points on the globe.
}
\usage{
earth.bear(lat1, long1, lat2, long2)
}
\arguments{
  \item{lat1}{Latitude of site 1}
  \item{long1}{Longitude of site 1}
  \item{lat2}{Latitude of site 2}
  \item{long2}{Longitude of site 2}
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
##add examples
}
\keyword{manip}
