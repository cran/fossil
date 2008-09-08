\name{bootstrap}
\alias{bootstrap}
\title{Bootstrap species estimators}
\description{
  Computes the bootstrap species estimator for abundance or presence-absence data
}
\usage{
bootstrap(x, taxa.row = TRUE, abund = TRUE, samples = NA)
}
\arguments{
  \item{x}{a vector, matrix or data frame of positive integers or zero of any size}
  \item{taxa.row}{whether each row of the matrix is a different taxon}
  \item{abund}{whether the input is abundance (or presence/absence) based}
  \item{samples}{if input is a vector file, the number of samples must be
  included}
}
\details{
  These functions compute the Chao 1 and Chao 2 richness estimators, respectively.
  Both functions will accept a vector, matrix or data frame of any size made up
  of positive integers and zeros. Matrices are by default treated such that each
  row is a different taxon and each column is a sample or locality, however if
  they are arranged with the taxa as columns, change the argument
  \code{taxa.row} to FALSE. This funtion is currently under development, and may
  change with the following release.
}
\value{
  Returns a single value for the Bootstrap Species Estimator
}
\references{Smith, E.P. & van Belle, G. 1984. Nonparametric estimation of species richness. Biometrics 40, 119-129.}
\author{Matthew Vavrek}

\seealso{\code{\link{jack1}}}
\examples{
## sample vector
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
bootstrap(a,samples=45)

## matrix format
a<-matrix(c(0,5,1,1,2,0,0,1,0,0,8,45),4,3)
bootstrap(a)
bootstrap(a,,FALSE)


## presence absence matrix
a<-matrix(c(0,1,1,1,1,0,0,1,0,0,1,1),4,3)
bootstrap(a,,FALSE)
}

\keyword{manip}
