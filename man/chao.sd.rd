\name{chao.sd}
\alias{chao.sd}
\title{Chao's estimation of standard error}
\description{
  Computes the standard error for \code{chao1} or \code{chao2}
}
\usage{
chao.sd(x)
}

\arguments{
  \item{x}{a vector of abundances or frequencies of occurrences}
}
\details{
  primarily designed to be used internally by \code{spp.est} to calculate the 
  errors for the chao estimators
}
\value{
  returns a value for standard deviation for \code{chao1} or \code{chao2}
}
\references{
  }
\author{Matthew Vavrek}
\seealso{\code{\link{chao1}}, \code{\link{spp.est}}}
\examples{
##add examples
}
\keyword{manip}