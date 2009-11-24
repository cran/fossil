\name{ace}
\alias{ACE}
\alias{ICE}
\title{Abundance- and Incidence-based Coverage Estimators}
\description{
  Computes the extrapolated species richness of a population using the Abundance-
  and Incidence-based Coerage Estimators
}
\usage{
ACE(x, taxa.row = TRUE)
ICE(x, taxa.row = TRUE)
}

\arguments{
  \item{x}{a vector, matrix or data frame of positive integers or zero of any size}
  \item{taxa.row}{whether each row of the matrix is a different taxon; if so, value is T}
}
\details{
  These functions compute the ACE and ICE richness estimators, respectively.
  Both functions will accept a vector, matrix or data frame of any size made up of positive
  integers and zeros. Matrices are by default treated such that each row is a
  different taxon and each column is a sample or locality, however if they are
  arranged with the taxa as columns, change the argument \code{taxa.row} to FALSE
  Take note that \code{ACE} is intended only for use with abundance data, and not
  presence absence data. While \code{ICE} will accept abundance matrices, it
  will internally convert the matrix to presence absence data. Note that if 
  \code{ACE} returns NaN or Inf as a value, that Chao1 will be used in it's place
  as per the recommendation made by Colwell in EstimateS.
}
\value{
  A value representing a minimum number of species present in the assemblage if 
  the entire population were to be censused.
}
\references{
Chao, A., M.-C. Ma, & M. C. K. Yang. 1993. Stopping rules and estimation for 
  recapture debugging with unequal failure rates. Biometrika 80, 193-201.

Chao, A., W.-H. Hwang, Y.-C. Chen, and C.-Y. Kuo. 2000. Estimating the number of
  shared species in two communities. Statistica Sinica 10:227-246.

Chazdon, R. L., R. K. Colwell, J. S. Denslow, & M. R. Guariguata. 1998.
  Statistical methods for estimating species richness of woody regeneration in
  primary and secondary rain forests of NE Costa Rica. Pp. 285-309 in F.
  Dallmeier and J. A. Comiskey, eds. Forest biodiversity research, monitoring
  and modeling: Conceptual background and Old World case studies. Parthenon
  Publishing, Paris.}
\author{Matthew Vavrek, with recommendations from the EstimateS reference manual by R.K. Colwell}
\seealso{For related species estimators, see \code{\link{chao1}}, \code{\link{bootstrap}} and \code{\link{jack1}}}
\examples{
## sample vector
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
ACE(a)
ICE(a)

## matrix format
a<-matrix(c(0,5,1,1,2,0,0,1,0,0,8,45),4,3)
ACE(a)
ICE(a)

## presence absence matrix
a<-matrix(c(0,1,1,1,1,0,0,1,0,0,1,1),4,3)
ACE(a)
ICE(a)

}

\keyword{manip}
