\name{jack1}
\alias{jack1}
\alias{jack2}
\title{First- and second-order jacknife estimators}
\description{
  Computes the extrapolated species richness of a population using first- or
  second-order jacknife stimators
}
\usage{
jack1(x, taxa.row = TRUE, abund = TRUE)
jack2(x, taxa.row = TRUE, abund = TRUE)
}

\arguments{
  \item{x}{a vector, matrix or data frame of positive integers or zero of any size}
  \item{taxa.row}{whether each row of the matrix is a different taxon; if so, value is set to TRUE}
  \item{abund}{If true, data is assumed to be abundance, if false, presence
    absence is assumed}
}
\details{
  These functions compute the first and second-order jacknife species richness
  estimators, respectively. Both functions will accept a vector, matrix or data
  frame of any size made up of positive integers and zeros. Matrices are by
  default treated such that each row is a different taxon and each column is a
  sample or locality, however if they are arranged with the taxa as columns,
  change the argument \code{taxa.row} to FALSE. If the data is abundance based,
  \code{abund} should be set to TRUE. If \code{abund} is set to FALSE, the data
  will be converted to presence/absence if not already in that format. For
  single vectors/columns, \code{taxa.row} and \code{abund} are ignored.
}
\value{
  The value returned is the Jackknife estimated species diversity of the dataset in question.
}
\references{Burnham, K.P. & W.S. Overton. 1978. Estimation of the size of a closed population when capture probabilities vary among animals. Biometrika 65, 623-633.

Burnham, K.P. & W.S. Overton. 1979. Robust estimation of population size when capture probabilities vary among animals. Ecology 60, 927-936.

Heltshe, J. & Forrester, N.E. 1983 . Estimating species richness using the jackknife procedure. Biometrics 39, 1-11.
 }
\author{Matthew Vavrek}

\seealso{\code{\link{ACE}}}
\examples{
## sample vector
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
jack1(a)

## matrix format
a<-matrix(c(0,5,1,1,2,0,0,1,0,0,8,45),4,3)
jack1(a)
jack2(a)
jack2(a,abund = FALSE)

## presence absence matrix of the above abundance matrix
a<-matrix(c(0,1,1,1,1,0,0,1,0,0,1,1),4,3)
jack1(a)
jack2(a)
jack2(a, abund = FALSE)
}

\keyword{manip}
