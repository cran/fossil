\name{earth.dist}
\alias{earth.dist}
\title{Calculating Geographic Distances}
\description{
  Create a distance matrix (lower triangle) between a list of points
}
\usage{
earth.dist(lats, dist = TRUE)
}
\arguments{
  \item{lats}{a table with a longitude and latitude column respectively as the 
  first two columns}
  \item{dist}{A logical argument whether to create a distance matrix (lower 
  triangle) or full matrix}
}
\details{
  This function will calculate the pairwise distances between all points 
  given and return either a distance or full matrix as specified. All coordinates 
  must be in decimal degrees.
}
\value{
  Returns a matrix of distances in kilometers between a list of longitudes and 
  latitudes.
}
\author{Matthew Vavrek}
\note{
  Large datasets may take some time to process, as the number of distances 
  to calculate is factorial in nature.
}
\seealso{\code{\link{deg.dist}}}
\examples{
data(fdata.lats)
earth.dist(fdata.lats)
}
\keyword{datagen}

