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
\item{lats}{a table with a longitude and latitude column respectively as the first two columns, or a SpatialPoints object with longitude/latitude}
\item{spp}{A matrix/data frame of species (rows) by samples/localities (columns)}
}
\details{
This will take a set of geographic coordinates along with a table of species by localities and return a list consisting of a matrix (\code{$areavsspp}) with a column of total area and of total species present, and a vector (\code{ranks}) with the order the samples were added in. The area is calculated by starting with the most central point, and adding those points closest to it, calculating a minimum spanning polygon as each new site is added, until all points are used.
}
\value{
  Returns a list of a matrix with columns of total area and total species recorded respectively and a vector of sample orders.
}
\author{Matthew Vavrek}
\seealso{\code{\link{earth.dist}}, \code{\link{earth.poly}}}
\examples{
#fdata species/area relationship
data(fdata.lats)
data(fdata.mat)
a<-sac(fdata.lats, fdata.mat)
plot(log(a$areavsspp))
}
\keyword{datagen}
