\name{euler.rot}
\alias{euler.rot}
\title{Calculate the Euler Rotation of a Point}
\description{
  Calculate the rotation of a point on the Earth for a given Euler pole. The rotation assumes a shperical earth.
}
\usage{
euler.rot(lat1, long1, rotdeg, lat2, long2)
}
\arguments{
  \item{lat1}{Euler-pole latitude}
  \item{long1}{Euler-pole longitude}
  \item{rotdeg}{Rotation about Euler-pole}
  \item{lat2}{Latitude of point to be converted}
  \item{long2}{longitude of point to be converted}
}
\details{
  Locations of the Euler pole and the point to be rotated must be given in decimal degrees.
}
\value{
  Rotated latitude and longitude of the provided point in decimal degrees.
}
\references{ ~put references to the literature/web site here ~ }
\author{Matthew Vavrek}

