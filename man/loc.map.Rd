\name{loc.map}
\alias{loc.map}
\title{Mapping Points on a Global Map}
\description{
  A function to plot any number of points given their latitude and longitude 
  respectively on a map of the world.
}
\usage{
loc.map(x, ...)
}
\arguments{
  \item{x}{a table with a latitude and longitude column respectively with 
  optional associated attributes}
  \item{\dots}{arguments to be passed to the \code{plot} call}
}
\details{
  This is a helper function, which automatically zooms in and centers the map 
  view on the input points. The \code{\dots} allow the user to adjust the usual 
  parameters for a scatterplot outlined by \code{\link{par}}. 
}
\value{
 Plots a map of the world focused on the locations provided.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{msn.map}}}
\examples{
##add examples
}

\keyword{graphs}

