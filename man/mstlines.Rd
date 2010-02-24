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
A function to plot the lines of a minimum spanning tree/forest on a plot; works as a frontend for \code{lines}.}
\author{Matthew Vavrek}
\seealso{\code{\link{dino.dist}}}
\examples{
#plot with overlain MST for fdata dataset
data(fdata.lats)
data(fdata.mat)
fdata.dist<-dino.dist(fdata.mat)
fdata.mst<-dino.mst(fdata.dist)
plot(coordinates(fdata.lats))
mstlines(fdata.mst, coordinates(fdata.lats))
}

\keyword{datagen}
