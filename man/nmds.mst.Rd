\name{nmds.mst}
\alias{nmds.mst}
\title{Creating NMDS plots with overlain Minimum Spanning Trees}
\description{
  This is a helper function which will plot an NMDS with an overlain MST
}
\usage{
nmds.mst(nmds, mst, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nmds}{an NMDS created using the ecodist program}
  \item{mst}{a minimum spanning tree or network (binary matrix)}
  \item{\dots}{arguments to be passed to the \code{plot} function}
}
\details{
At the moment, the function requires an NMDS created using the ecodist program, howver the minimum spanning tree can be any one which creates a binary matrix showing connections (ie \code{\link{dino.mst}}).
}
\value{
  Plots a non-metric multidimensional scaling plot with an overlain minimum 
  spanning tree showing connections between the points.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{dino.msn}}, \code{\link{dino.mst}}}
\examples{
##add examples

}

\keyword{graphs}
