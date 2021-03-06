\name{chao1}
\alias{chao1}
\alias{chao2}
\title{Chao's Species Estimators}
\description{
  Computes the Chao species estimator for abundance or presence-absence data
}
\usage{
chao1(x, taxa.row = TRUE)
chao2(x, taxa.row = TRUE)
}

\arguments{
  \item{x}{a vector, matrix or data frame with species by samples}
  \item{taxa.row}{a logical argument if the species are the rows or columns}
}
\details{
\code{chao1} will return an estimate of species richness based on a vector or matrix of abundance data, while \code{chao2} will return an estimate of species richness based on incidence data. Note that \code{chao1} estimator is for abundance data only. The \code{chao2} estimator can be given abundance data and it will automagically convert it to incidence data, but due to the nature of the estimator, the data must contain more than one sample (ie the data must be arranged in a minimum 2 by 2 matrix).
}
\value{
  returns a value for the Chao Species Estimator for a the given data.
}
\note{
While the function will still return a value, if all the species abundances are equal to 1 in the input to \code{chao1}, a warning will be raised, and the value returned will be equal to the number of species observed.
}
\references{
  Chao, A. 1984. Non-parametric estimation of the number of classes in a 
  population. Scandinavian Journal of Statistics 11: 265-270.

  Chao, A. 1987. Estimating the Population Size for Capture-Recapture Data 
  with Unequal Catchability. Biometrics 43: 783-791.
}
\author{Matthew Vavrek}
\seealso{\code{\link{jack1}}, \code{\link{bootstrap}}}
\examples{
## sample vector
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
chao1(a)

## matrix format
a<-matrix(c(0,5,1,1,2,0,0,1,0,0,8,45),4,3)
chao1(a)
chao2(a)

## presence absence matrix
a<-matrix(c(0,1,1,1,1,0,0,1,0,0,1,1),4,3)
chao1(a)
chao2(a)
}
\keyword{manip}
