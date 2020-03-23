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
Colwell, R.K. 2010. EstimateS: Statistical estimation of species richness and shared species from samples. Version 8.2. User's Guide and application published at: http://purl.oclc.org/estimates.
}
\author{Matthew Vavrek}
\seealso{\code{\link{chao1}}, \code{\link{spp.est}}}
\examples{
## sample vector
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
chao.sd(a)

}
\keyword{manip}
