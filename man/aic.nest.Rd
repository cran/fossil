\name{aic.nest}
\alias{aic.nest}
\title{Nestedness of samples using AIC}
\description{
  Test if two empirical samples are drwan from the same or different communities
}
\usage{
    aic.nest(comm1, comm2, base=exp(1))
}
\arguments{
  \item{comm1, comm2}{lists of abundance data from two empirical samples}
  \item{base}{base of the log used in the calculation of Shannon's diversity}
}
\details{
This function tests if two empirical samples are drawn from the same community, based on the AIC scores.
}
\value{
returns two AIC scores, the first assuming the two empirical samples are drawn from the same distribution, the other assuming the two empirical samples are from different distributions
}
\author{Matthew Vavrek}

\seealso{\code{\link{simpson}}}
\examples{
#for example, two different communities
a<-c(12,4,12,1,4,0,6,5,0,0,0)
b<-c(0,11,4,3,6,7,7,2,23,5,8)

#if the aic score is lower, it is the better model
aic.nest(a,b)

#from the same community
a<-c(5,6,5,6,5,6,5,6,5,2,1,1)
b<-c(2,3,2,3,2,3,2,3,2,1,0,0)
aic.nest(a,b)
}
\keyword{stats}

