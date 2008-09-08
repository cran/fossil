\name{msn.map}
\alias{msn.map}
\title{Mapping a Minimum Spanning Tree}
\description{
 Creating a quick and focused map using a world map for gegraphically referenced 
 visualization within R of a minimum spanning tree or network.
}
\usage{
msn.map(msn, lat, ...)
}
\arguments{
  \item{msn}{minimum spanning tree or network to be used}
  \item{lat}{the lats}
  \item{\dots}{arguments to be passed to \code{plot}}
}
\details{
This is a helper function for quick visualization of georeferenced minimum spanning trees, and is not meant for creating figure quality images due to lack of fine control over many functions
}
\value{
Returns a map of the globe, focused in on any set of georeferenced localities.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{dino.msn}}}
\examples{
##add examples
}
\keyword{graphs}
