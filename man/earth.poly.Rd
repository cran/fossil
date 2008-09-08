\name{earth.poly}
\alias{earth.poly}
\title{Calculating a Minimum Convex Polygon}
\description{
  Calculate a minimum convex polygon for a collection of points without knowing 
  what points form the vertices.
}
\usage{
earth.poly(x, plot.map = FALSE, ...)
}

\arguments{
  \item{x}{a table with a latitude and longitude column respectively as the 
  first two columns}
  \item{plot.map} {A logical argument whether to plot the points on a map; default is FALSE} 
  \item{\dots}{plotting arguments to be passed to the map plotting function; 
  will be ignored if \code{plot.map}=FALSE}
}
\details{
  This function will plot a 
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\note{
}
\seealso{\code{\link{earth.tri}}}
\examples{
##add example
}

\keyword{graphs}

