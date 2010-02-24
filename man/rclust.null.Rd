\name{rclust.null}
\alias{rclust.null}
\title{Relational Clustering}
\description{
  A method to cluster a number of samples using a relational (dissimila
}
\usage{
rclust.null(groups, dist)
}
\arguments{
  \item{groups}{cluster identity vector}
  \item{dist}{original pairwise distance matrix}
}
\details{
This function calculates a random/null clustering solution based on a given solution. It resamples the data and reassigns samples to groups, keeping the same group sizes.
}
\value{
  A matrix equal in rows to the number of clusters originally given, with two columns for mean within group distance and standard deviation respectively.
}
\author{Matthew Vavrek}

\seealso{
To cluster the data, use the function \code{\link{rclust}}; see as well \code{\link{rclust.weights}}, \code{\link{rclust.dist}}
}
\examples{
#a null solution for the fdata example data set
data(fdata.mat)
fd.dist <- dino.dist(fdata.mat)
fd.clust <- rclust(fd.dist, 2)
rclust.null(fd.clust, fd.dist)
}
\keyword{data}
