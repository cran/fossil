\name{sac}
\alias{sac}
\title{Calculate Species Area Curves}
\description{
  Calculating a species area curve for a set of georeferenced localities
}
\usage{
sac(lats, spp)
}
\arguments{
  \item{lats}{a table with latitude and longitude respectively for each locality}
  \item{spp}{A matrix/data frame of species (rows) by samples/localities (columns)}
}
\details{
  This will take a set of geographic coordinates along with a table of species 
  by localities and return a matrix with a column of total area and of total 
  species present. The area is calculated by starting with the most central 
  point, and adding those points closest to it, calculating a minimum spanning 
  polygon as each new site is added, until all points are used.
}
\value{
  Returns a matrix with columns of total area and total species recorded, 
  respectively.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{earth.dist}}, \code{\link{earth.poly}}}
\examples{
##add examples
}
\keyword{datagen}
