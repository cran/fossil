\name{similarity}
\alias{bray.curtis}
\alias{euclidean}
\alias{jaccard}
\alias{simpson}
\alias{sorenson}
\alias{manhattan}
\alias{morisita.horn}
\alias{ochiai}
\alias{kulczynski}
\alias{braun.blanquet}
\title{Similarity/Dissimilarity Indices}
\description{
  Functions to calculate the ecological distance between two groups
}
\usage{
braun.blanquet(x, y)
bray.curtis(x, y)
euclidean(x,y)
kulczynski(x,y)
jaccard(x, y)
manhattan(x, y)
morisita.horn(x, y)
ochiai(x, y)
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
  found should be coded as a zero. For details on each index, please consult the references.
}
\value{
  Returns the similarity index for the two sites in question. 
}
\references{

Shi, G. R. 1993. Multivariate data analysis in palaeoecology and palaeobiogeography -- a review. Palaeogeography, Palaeoclimatology, Palaeoecology 105: 199--234.

Magurran, A. E. 2004. Measuring Biological Diversity. Oxford, Blackwell.

}
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

