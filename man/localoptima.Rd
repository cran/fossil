\name{localoptima}
\alias{localoptima}
\title{Function to Find Local Optimization for clustering}
\description{
  A function meant to be used internally be the \code{relational.clustering} function
}
\usage{
localoptima(dist, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{Distance matrix to be used}
  \item{group}{group designations}
}
\details{
The function takes a distance matrix and a vector with the group identifications for each sample locality (or equivalent). It is mainly meant to be used internally by the \code{relational.clustering} function to optimize the initial clustering and find the local (which hopefully is also the global) optimal organization, such that each member of a group is more similar to the other members in it's group (on average) than to any other groups.
}
\value{
  Arc distance between two points on the Earth's surface in kilometers.
}
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
