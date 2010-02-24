\name{lats2Shape}
\alias{lats2Shape}
\title{Converting a Table of Latitudes and Longitudes to a Shapefile}
\description{
  A helper function to convert a table of latitudes and longitudes (and 
  associated attributes, if applicable) into a shapefile
}
\usage{
lats2Shape(lats)
}
\arguments{
  \item{lats}{a table with a latitude and longitude column respectively with associated attributes}
}
\details{
  The table to be converted must contain as it's first two columns the latitude 
  (or Y) and longitude (or X) values to be converted. Any other number of 
  columns in any format can also be attached, and will be included in the 
  attribute table.
}
\value{
  A shapefile object which can be written to file using \code{write.shapefile}
}
\author{Matthew Vavrek}
\seealso{\code{\link{msn2Shape}}}
\examples{
\dontrun{
#use fdata.lats as dataset
data(fdata.lats)
shape.lats<-lats2Shape(fdata.lats)
write.shapefile(shape.lats, file='/path/to/write/lats')
}
}
\keyword{datagen}

