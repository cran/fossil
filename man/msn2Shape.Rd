\name{msn2Shape}
\alias{msn2Shape}
\title{Convert a Minimum spanning Network or Tree to Shapefile}
\description{
  A helper function to convert a minimum spanning tree or network into shapefile 
  format. 
}
\usage{
msn2Shape(msn, lat, dist = NULL)
}
\arguments{
  \item{msn}{a minimum spanning tree or network (binary matrix)}
  \item{lat}{a matrix or data frame with the latitude and longitude of the 
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
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{lats2Shape}} for a function to convert a lat/long table to 
  a shapefile}
\examples{
##add example for this function
}
\keyword{datagen}

