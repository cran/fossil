\name{rclust}
\alias{rclust}
\title{Relational Clustering}
\description{
  A method to cluster a number of samples using a relational (dissimila
}
\usage{
rclust(dist, clusters = 2, runs = 10, counter = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{pairwise distance matrix}
  \item{clusters}{final number of clusters}
  \item{runs}{number of randomizations to run}
  \item{counter}{use counter to display current run number}
}
\details{
This function will return a cluster identity vector. The number of clusters requested must be 2 or greater, but 1/2 or less than the total number of samples, as the function requires at least 2 samples per cluster.
}
\value{
The vector returned will be the same length as the number of samples provided in the original \code{dist} matrix. The samples will have been clustered into the same number of clusters as defined in \code{clusters}
}
\author{Matthew Vavrek}
\seealso{
\code{\link{rclust.dist}}, \code{\link{rclust.null}}, \code{\link{rclust.weights}} and \code{\link{coi}} 
}
\examples{
#a null solution for the fdata example data set
data(fdata.mat)
fd.dist <- dino.dist(fdata.mat)
rclust(fd.dist, clusters = 2, runs = 10)
}
\keyword{data}
