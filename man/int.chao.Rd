\name{int.chao}
\alias{int.chao}
\title{Internal function for chao estimators}
\description{
  Computes the Chao species estimator for both \code{chao1} and \code{chao2}
  estimators
}
\usage{
int.chao(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of positive integers or zero of any length}
}
\details{
  This function is typically only called internally by the functions \code{chao1}
  and \code{chao2}. The function has a built in bias correction, such that it
  will not return values of infinity or non-numbers.
}
\value{
  Estimated numer of species using the Chao estimator.
}
\references{Chao, A. 1984. Nonparametric estimation of the number of classes in a population.
  Scandinavian Journal of Statistics 11: 265-270.}
\author{Matthew Vavrek}
\seealso{For the more useful implementations of the Chao estimator, see \code{\link{chao1}} for the abundance based estimator or \code{\link{chao2}} for the incidence based estimator}
\examples{
## create example data set
a<-c(4,5,1,1,2,0,0,1,3,0,8,45,23)
int.chao(a)

## a data set which would give NaN using classic (ie not bias corrected) version
a<-c(4,5,0,0,2,0,0,0,3,0,8,45,23)
int.chao(a)
}
\keyword{manip}
