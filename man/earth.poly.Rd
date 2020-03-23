\name{earth.poly}
\alias{earth.poly}
\title{Calculating a Minimum Convex Polygon}
\description{
  Calculate a minimum convex polygon for a collection of points without knowing 
  what points form the vertices.
}
\usage{
earth.poly(lats)
}

\arguments{
\item{lats}{a table with a longitude and latitude column respectively as the first two columns, or a SpatialPoints object with longitude/latitude}
}
\details{
  This function will calculate the area of a minimum convex polygon/convex hull for a spherical surface (ie points on a globe).
}
\value{
The function will return a list consisting of the area in $km^2$ (\code{\$area}) and a vector with the row numbers of the vertices (\code{\$vertices})
}
\author{Matthew Vavrek}
\seealso{\code{\link{earth.tri}}}
\examples{
#1/8th the surface area of the earth
a <- matrix(c(0, 0, 0, 90, 90, 0, 25, 25), 4, 2, byrow = TRUE)
earth.poly(a)
}

\keyword{graphs}

