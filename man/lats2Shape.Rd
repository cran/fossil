\name{lats2Shape}
\alias{lats2Shape}
\title{Converting a Table of Latitudes and Longitudes to a Shapefile}
\description{
  A helper function to convert a table of latitudes and longitudes (and 
  associated attributes, if applicable) into a shapefile
}
\usage{
lats2Shape(x)
}
\arguments{
  \item{x}{a table with a latitude and longitude column respectively with associated attributes}
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
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}
\seealso{\code{\link{msn2Shape}}}
\examples{
##add examples
}
\keyword{datagen}

