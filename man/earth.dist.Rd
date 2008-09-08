\name{earth.dist}
\alias{earth.dist}
\title{Calculating Geographic Distances}
\description{
  Create a distance matrix (lower triangle) between a list of points
}
\usage{
earth.dist(x, dist = TRUE)
}
\arguments{
  \item{x}{a table with a latitude and longitude column respectively as the 
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
  Returns a matrix of distances in kilometers between a list of latitudes and 
  longitudes.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\note{
  Large datasets may take some time to process, as the number of distances 
  to calculate is factorial in nature.
}
\seealso{\code{\link{deg.dist}}}
\examples{
##area of 1/8th of the earth
a <- matrix(c(0,0,90,0,90,0),3,3)
earth.dist(a)
earth.dist(a, FALSE)
}
\keyword{datagen}

