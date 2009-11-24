\name{bray.curtis}
\alias{bray.curtis}
\alias{jaccard}
\alias{simpson}
\alias{sorenson}
\alias{morisita.horn}
\title{Similarity Indices}
\description{
  Functions to calculate the similarities between two groups
}
\usage{
bray.curtis(x, y)
jaccard(x, y)
morisita.horn(x, y)
simpson(x, y)
sorenson(x, y)
}
\arguments{
  \item{x}{species from group A}
  \item{y}{species from group B}
}
\details{
  You must provide two separate vectors, with species arranged in the same 
  order, from area A and B. If species are present in one site but not the 
  other, these must be recorded for both sites; the site where they are not 
  found should be coded as a zero. Species not present at either site are 
  ignored. For details on each index, please consult the references.
}
\value{
  Returns the similarity index for the two sites in question. 
}
\references{Magurran, A. E. (2004). Measuring Biological Diversity. Oxford, Blackwell.}
\author{Matthew Vavrek}

\seealso{\code{\link{dino.dist}}}
\examples{
##Species counts from two different locations
a <- c(1,0,4,3,5,0,0,7)
b <- c(2,1,3,0,0,1,0,6)
bray.curtis(a,b)
jaccard(a,b)
simpson(a,b)
sorenson(a,b)
morisita.horn(a,b)
}
\keyword{manip}

