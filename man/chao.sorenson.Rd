\name{chao.sorenson}
\alias{chao.sorenson}
\alias{chao.jaccard}
\title{Chao's Jaccard and Sorenson Estimators of Shared Species}
\description{
  Chao's Jaccard and Sorenson shared species estimators for use with incomplete datasets
}
\usage{
chao.sorenson(x, y)
chao.jaccard(x, y)
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
  ignored.
}
\value{
  Returns the Chao-Jaccard or Chao-Sorenson similarity index for the two sites 
  in question.
}
\references{
  Chao, A., R. L. Chazdon, et al. (2005). A new statistical approach 
  for assessing similarity of species composition with incidence and abundance 
  data. Ecology Letters 8(2): 148-159.
}
\author{Matthew Vavrek}
\seealso{\code{\link{bray.curtis}}}
\examples{
##add examples
}
\keyword{manip}
