\name{coi}
\alias{coi}
\title{Cohesiveness Index for Relational Clustering}
\description{
  Computes Cohesiveness Index for a Cluster Analysis
}
\usage{
coi(mst, groups)
}

\arguments{
  \item{mst}{A minimum spanning tree matrix (binary)}
  \item{groups}{A vector with the group/cluster assignments for each sample}
}

\value{
  Returns a something
}
\note{
While the function will still return a value, if all the species abundances are equal to 1 in the input to \code{chao1}, a warning will be raised, and the value returned will be equal to the number of species observed.
}
\author{Matthew Vavrek}
\seealso{\code{\link{rclust}}}
\examples{
## sample vector
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
chao1(a)

## matrix format
a<-matrix(c(0,5,1,1,2,0,0,1,0,0,8,45),4,3)
chao1(a)
chao2(a)

## presence absence matrix
a<-matrix(c(0,1,1,1,1,0,0,1,0,0,1,1),4,3)
chao1(a)
chao2(a)
}
\keyword{manip}
