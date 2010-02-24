\name{create.lats}
\alias{create.lats}
\title{Creating a table of Latitudes and Longitudes}
\description{
  Create a matrix of locations with a column of latitudes and longitudes
}
\usage{
  create.lats(x, loc="locality", long="longitude", lat="latitude")

}
\arguments{
  \item{x}{a table arranged in columnar format, with one column
    indicating the locations, another the latitude and another the longitude}
  \item{loc}{the name or number of the column giving the names  of the
  locations to be used}
  \item{long}{the name or number of the column giving the longitude of the
  locations}
  \item{lat}{the name or number of the column giving the latitude of the
  locations}
}
\details{
  This function will create a location table with longitude (X) and latitude (Y) or their equivalents
  for every location. This function ceates a matrix in the format needed for most of the geographic functions found in the fossil package.
}
\value{
  A matrix with a column of longitude and latitude, respectively with rownames
  correspnding to each location
}
\author{Matthew Vavrek}
\seealso{\code{\link{create.matrix}}}
\examples{
#to reproduce the fdata.lats dataset
data(fdata.list)
create.lats(fdata.list, loc="locality", long="longitude", lat="latitude")
}

\keyword{data}
