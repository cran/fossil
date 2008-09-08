\name{dino.mst}
\alias{dino.mst}
\alias{dino.msn}
\title{Calculate a Minimum Spanning Tree or Network}
\description{
  Methods for calculating a minimum spanning tree or network between a number of 
  points given a distance matrix.
}
\usage{
dino.mst(x, random.start = TRUE, random.search = TRUE)
dino.msn(x)
}
\arguments{
  \item{x}{a distance matrix for any number of points}
  \item{random.start}{If the minimum spanning tree is to start at a random point and not the first given site (default is TRUE)}
  \item{random.search}{If there is more than one shortest possible branch, should one be chosen randomly}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a binary matrix where connections between points are denoted by a 1.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{dino.dist}}}
\examples{
##add example
}

\keyword{datagen}
