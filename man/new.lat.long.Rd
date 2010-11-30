\name{new.lat.long}
\alias{new.lat.long}
\title{Find a New Latitude and Longitude}
\description{
  Find a new location using an original location (latitude and longitude) along with a bearing and distance
}
\usage{
new.lat.long(long, lat, bearing, distance)
}
\arguments{
  \item{long}{original longitude}
  \item{lat}{original latitude}
  \item{bearing}{bearing from original point to new location, degrees from North}
  \item{distance}{distance to location}
}
\value{
  a vector of length 2 with the new latitude and longitude respectively
}
\author{Matthew Vavrek}
\seealso{\code{\link{deg.dist}}, \code{\link{earth.bear}}}
\examples{
#Travel from 0,0 to a new location at a bearing of 45 degrees from North (clockwise) and 1000 km away
new.lat.long(long = 0, lat = 0, bearing = 45, distance = 1000)
}
