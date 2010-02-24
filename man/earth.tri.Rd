\name{earth.tri}
\alias{earth.tri}
\title{Calculating the Surface Area Enclosed by Three Geographic Points}
\description{
  Calculate the true area on a sphere enclosed by three points on the earth's surface
}
\usage{
earth.tri(long1, lat1, long2, lat2, long3, lat3)
}
\arguments{
  \item{long1}{Longitude of site 1}
  \item{lat1}{Latitude of site 1}
  \item{long2}{Longitude of site 2}
  \item{lat2}{Latitude of site 2}
  \item{long3}{Longitude of site 3}
  \item{lat3}{Latitude of site 3}
}
\details{
  A function to find the area enclosed by three points on the surface of the 
  earth, given their latitudes and longitudes. This function is primarily 
  designed to be a component of \code{\link{earth.poly}}, which is likely a more 
  useful function for most applications.
}
\value{
  Returns a value in kilometers squared of the area enclosed by the three points.
}
\references{Wolfram Mathworld, \url{http://mathworld.wolfram.com/SphericalTriangle.html}}
\author{Matthew Vavrek}
\note{
The distance calculated may be up to 0.2\% inaccurate, as this function treats the Earth as a sphere with a circumference of 40041.47 km (mean circumference), rather than an ellipsoid like it actually is. 
}
\seealso{\code{\link{earth.poly}}}
\examples{
#1/8th the surface area of the earth
earth.tri(0, 0, 0, 90, 90, 0)

}
\keyword{datagen}
