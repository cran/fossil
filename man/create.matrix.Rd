\name{create.matrix}
\alias{create.matrix}
\title{Creating species locality matrices}
\description{
  Create a matrix with taxa as rows and occurrences or samples as columns
}
\usage{
  create.matrix(x,tax.name="genus",locality="locality",time.col=NULL,time=NULL,abund=FALSE,abund.col="abundance")

}
\arguments{
  \item{x}{a table arranged in columnar format, with at least one column
    indicating name of taxa and another giving location or sample}
  \item{tax.name}{the name or number of the column giving the taxonomic names to 
  be used (the rows of the matrix to be created)}
  \item{locality}{the name or number of the column giving the locations of the 
  samples (the columns of the matrix to be created)}
  \item{time.col}{what is the column name or number containing the time periods; 
  if left null, filtering for time willbe ignored}
  \item{time}{what time periods to keep for the matrix; if left null, filtering 
  for time willbe ignored}
  \item{abund}{whether to record abundances of taxa; if left FALSE, a binary
  (presence/absence) matrix is created}
  \item{abund.col}{column name or number containing abundance values}
}
\details{
  This is a helper function to convert large lists of data into matrices of 
  species (rows) and locations (columns). The parameters can be adjusted to 
  create either a binary (presence/absence) or abundance matrix. The setup of 
  the table is largely flexible; simply input the column names or numbers 
  containing the pertinent information. To filter data according to time, both 
  the time column and the time period must be specified. For abundance, the 
  default title for the abundance column is simply "abundance"; the function 
  will not work if you have chosen to include abundances (\code{abund = TRUE} 
  but the name of the abundance column is incorrect.}
\value{
  A matrix of taxa (rows) by localities (columns).
}
\note{
At present, the function will ignore rows where the taxon name is \code{NA}, \code{NULL}, \code{''} (empty character value) or \code{' '} (single space), as these labels typically represent an unknown taxa, which would be inappropriate to include in most analyses.} 
\author{Matthew Vavrek}
\seealso{\code{\link{create.lats}}}
\examples{}

\keyword{data}
