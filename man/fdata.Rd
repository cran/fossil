\name{fdata}
\alias{fdata.list}
\alias{fdata.mat}
\alias{fdata.lats}
\title{A Sample Species Abundance Dataset}
\description{
A simple hypothetical data set used in many of the examples.
}
\value{
There are 3 datasets, however 2 of them (\code{fdata.mat} and \code{fdata.lats}) derive from the first (\code{fdata.list}). \code{fdata.list} is a table with 5 columns descriing the sample site, species name, abundance, and location in latitude/longitude. \code{fdata.mat} is a 12 by 12 species abundance matrix (12 unique species and 12 unique samples/localities) that can be recreated from the original table of occurrences using the \code{create.matrix()} function; likewise, the \code{fdata.lats()} contains the locations of each of the samples, and can be created using the \code{create.lats()}
}
\author{Matthew Vavrek}
\examples{
data(fdata.list)
}
\keyword{data}
