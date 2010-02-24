\name{rclust.weights}
\alias{rclust.weights}
\title{Relational Clustering}
\description{
  A method to cluster a number of samples using a relational (dissimila
}
\usage{
rclust.weights(groups, dist)
}
\arguments{
  \item{groups}{cluster identity vector}
  \item{dist}{original pairwise distance matrix}
}
\details{
This function creates an n by c sized matrix, where n is the number of samples and c is the number of groups, of the average distances for each sample from itself to all the members of another group (\eqn{c_{i}}).
}
\value{
  A matrix of of size n (samples) by c (groups).
}
\author{Matthew Vavrek}
\seealso{
To cluster the data, use the function \code{\link{rclust}}; see as well \code{\link{rclust.dist}}, \code{\link{rclust.null}}
}
\examples{
data(fdata.mat)
fd.dist <- dino.dist(fdata.mat)
fd.clust <- rclust(fd.dist, 2)
rclust.weights(fd.clust, fd.dist)
}
\keyword{data}
