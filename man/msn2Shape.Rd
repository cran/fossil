\name{msn2Shape}
\alias{msn2Shape}
\title{Convert a Minimum spanning Network or Tree to Shapefile}
\description{
  A helper function to convert a minimum spanning tree or network into shapefile 
  format. 
}
\usage{
msn2Shape(msn, lats, dist = NULL)
}
\arguments{
  \item{msn}{a minimum spanning tree or network (binary matrix)}
  \item{lats}{a matrix or data frame with the latitude and longitude of the 
  sites as the first two columns respectively}
  \item{dist}{Optional argument to include distance values in final output; if 
  wanted, a distance matrix (lower triangle) with the localities in the same 
  order as in the MSN are required}
}
\details{
  This function will take a minimum spanning tree or network object, along with 
  the georeferenced locations of the sites, and convert it into a shapefile for 
  use with GIS. The \code{msn} argument requires a minimu spanning tree or 
  network object, and the lat argument requires some form of location for each 
  of the points, typically a matrix with latitude and longitude columns 
  respectively.
}
\value{
  A shapefile which can be output using the \code{write.shapefile} function for
  use with a GIS program.
}
\author{Matthew Vavrek}
\seealso{\code{\link{lats2Shape}} for a function to convert a lat/long table to 
  a shapefile}
\examples{
\dontrun{
#import both fdata.lats and fdata.mat
data(fdata.lats)
data(fdata.mat)
fdata.dist<-dino.dist(fdata.mat)
fdata.mst<-dino.mst(fdata.dist)
shape.mst<-msn2Shape(fdata.mst, fdata.lats)
write.shapefile(shape.mst, file='/path/to/write/mst')
}
}
\keyword{datagen}

