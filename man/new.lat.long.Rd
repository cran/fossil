\name{new.lat.long}
\alias{new.lat.long}
\title{Find a New Latitude and Longitude}
\description{
  Find a new location using an original location (latitude and longitude) along with a bearing and distance
}
\usage{
new.lat.long(lat, long, bearing, distance)
}
\arguments{
  \item{lat}{original latitude}
  \item{long}{original longitude}
  \item{bearing}{bearing from original point to new location}
  \item{distance}{distance to location}
}
\value{
  a vector of length 2 with the new latitude and longitude respectively
}
\author{Matthew Vavrek}
\seealso{\code{\link{deg.dist}}, \code{\link{earth.bear}}}
\examples{
##add examples
}
\keyword{manip}
