\name{earth.tri}
\alias{earth.tri}
\title{Calculating the Surface Area Enclosed by Three Geographic Points}
\description{
  Calculate the true area on a sphere enclosed by three points on the earth's surface
}
\usage{
earth.tri(lat1, long1, lat2, long2, lat3, long3)
}
\arguments{
  \item{lat1}{Latitude of site 1}
  \item{long1}{Longitude of site 1}
  \item{lat2}{Latitude of site 2}
  \item{long2}{Longitude of site 2}
  \item{lat3}{Latitude of site 3}
  \item{long3}{Longitude of site 3}
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
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\note{
  The distance calculated may be up to 0.2\% inaccurate, as this function 
  treats the Earth as a sphere with a circumference of 40003 km, rather than an
  ellipsoid like it actually is. 
}
\seealso{\code{\link{earth.poly}}}
\examples{
##add examples
}
\keyword{datagen}
