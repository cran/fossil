\name{rclust.dist}
\alias{rclust.dist}
\title{Relational Clustering}
\description{
  Provides a distance matrix intra- and inter-group average distances based on a clustering solution and a dissimilarity matrix. 
}
\usage{
rclust.dist(groups, dist)
}
\arguments{
  \item{groups}{cluster identity vector}
  \item{dist}{original pairwise distance matrix}
}
\details{
This function calculates a distance matrix for each cluster, giving the average within group pairwise distance and the average between group pairwise distance.
}
\value{
A matrix \eqn{c} by \eqn{c} in size, where \eqn{c} is the number of clusters
}
\author{Matthew Vavrek}

\seealso{To cluster the data, use the function \code{\link{rclust}}; see as well \code{\link{rclust.weights}}, \code{\link{rclust.null}}}
\examples{
#a null solution for the fdata example data set
data(fdata.mat)
fd.dist <- dino.dist(fdata.mat)
fd.clust <- rclust(fd.dist, 2)
rclust.dist(fd.clust, fd.dist)
}
\keyword{data}
