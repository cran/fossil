\name{dino.dist}
\alias{dino.dist}
\title{Creating a Distance Matrix}
\description{
  Create a distance matrix between any number of locations
}
\usage{
dino.dist(x, method = "sorenson", type = "dis")
}
\arguments{
  \item{x}{matrix of taxa (or equivalent data) in rows by columns of localities 
  (or equivalent)}
  \item{method}{the distance/similarity index to compute}
  \item{type}{if the matrix is to be a distance ('dis') or similarity ('sim) matrix}
}
\details{
  This will create a distance (or similarity) matrix using any of the provided 
  indices: \code{\link{sorenson}}, \code{\link{simpson}}, 
  \code{\link{bray.curtis}}, \code{\link{jaccard}}, \code{\link{morisita.horn}},
  \code{\link{chao.jaccard}} and \code{\link{chao.sorenson}}. Creating a 
  distance matrix will give a value of 1 for the most distantly related sites, 
  while similarity index will give a value of 1 for the most similar sites.
}
\value{
  A distance matrix (lower triangle) giving the pairwise distance indices 
  between all points. 
}
\author{Matthew Vavrek}
\note{                                
 To use a user generated distance index, type the name of the function to be 
 used for method, and the function will use that function instead. Note that 
 the function internally provides two equal length vectors at a time to the 
 distance calculation function. 
}
\seealso{\code{\link{sorenson}}, \code{\link{simpson}}, 
  \code{\link{bray.curtis}}, \code{\link{jaccard}}, \code{\link{morisita.horn}},
  \code{\link{chao.jaccard}} and \code{\link{chao.sorenson}}}
\examples{
##example using fdata.mat
data(fdata.mat)
dino.dist(fdata.mat)
dino.dist(fdata.mat,simpson,"sim")

}
\keyword{manip}
