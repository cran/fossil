\name{spp.est}
\alias{spp.est}
\title{Estimating Species Diversity}
\description{
  Estimate the diversity of a sample(s) using a number of species diversity 
  estimators.
}
\usage{
spp.est(x, rand = 10, abund = TRUE, counter = TRUE)
}
\arguments{
  \item{x}{A vector, matrix or data frame with species as rows and 
  locations/samples as columns}
  \item{rand}{The number of times to run the internal randomizations; default 
  is set to 10}
  \item{abund}{If the data is abundance or presence/absence; default is set to 
  TRUE for abundance}
  \item{counter}{Whether or not to provide a running total of progress of randomizations}
}
\details{
  This function will accept a vector, matrix or data frame of species by samples 
  and return a large matrix with various species estimation values.
}
\value{
Returns a table with the following column names if \code{abund=TRUE}:
  \item{N.obs}{Total sample size} 
  \item{S.obs}{Number of observed species}
  \item{S.obs(+95\%)}{95\% upper confidence interval}  
  \item{S.obs(-95\%)}{95\% lower confidence interval}  
  \item{Chao1}{Chao Species Estimation}
  \item{Chao1(upper)}{95\% upper confidence interval} 
  \item{Chao1(lower)}{95\% lower confidence interval}
  \item{ACE}{Abundance-based Coverage Estimator}
  \item{ACE(upper)}{95\% upper confidence interval} 
  \item{ACE(lower)}{95\% lower confidence interval}
  \item{Jack1}{First Order Jacknife Estimator}
  \item{Jack1(upper)}{95\% upper confidence interval}
  \item{Jack1(lower)}{95\% lower confidence interval}

Returns a table with the following column names if \code{abund=FALSE}:
  \item{N.obs}{Total sample size} 
  \item{S.obs}{Number of observed species}
  \item{S.obs(+95\%)}{95\% upper confidence interval}  
  \item{S.obs(-95\%)}{95\% lower confidence interval}  
  \item{Chao2}{Chao Species Estimation}
  \item{Chao2(upper)}{95\% upper confidence interval} 
  \item{Chao2(lower)}{95\% lower confidence interval}
  \item{ICE}{Incidence-based Coverage Estimator}
  \item{ICE(upper)}{95\% upper confidence interval} 
  \item{ICE(lower)}{95\% lower confidence interval}
  \item{Jack1}{First Order Jacknife Estimator}
  \item{Jack1(upper)}{95\% upper confidence interval}
  \item{Jack1(lower)}{95\% lower confidence interval}
  
  
}
\references{
  The original idea for a program similar to this came from the 
  extremely useful EstimateS program by Robert K. Colwell

  Colwell, R. K. 2005. EstimateS: Statistical estimation of species 
  richness and shared species from samples. Version 7.5. User's Guide and 
  application published at: http://purl.oclc.org/estimates.
}
\author{Matthew Vavrek}
\note{
  This function can be very long to run due to its iterative nature. The 
  randomizations are initially set to 10 so the process will run relatively 
  quickly, but a low value for randomizations will not give nicely smoothed 
  curves.
}
\seealso{\code{\link{chao1}}, \code{\link{jack1}}, \code{\link{bootstrap}}}
\examples{
##add examples
}
\keyword{datagen}
