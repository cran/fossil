\name{mstlines}
\alias{mstlines}
\title{Display a Minimum Spanning Tree or Network}
\description{
  a method of displaying a Minimum Spanning Tree/Network over a given set of points
}
\usage{
mstlines(mst, x, y = NULL, pts.names = NULL, ...)
}
\arguments{
  \item{mst}{a minimum spanning tree or network object}
  \item{x}{either a table with the first two columns that of the x and y coordinates respectively, or simply that of the x coordinate}
  \item{y}{an optional argument if the y coordinates were not given in argument x}
  \item{pts.names}{If there is more than one shortest possible branch, should one be chosen randomly}
  \item{...}{arguments to be passed to lines()}
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
