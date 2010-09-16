\name{rand.index}
\alias{rand.index}
\alias{adj.rand.index}
\title{Rand Index and Adjusted Rand Index}
\description{
  Measures to compare the similarity of two clustering outcomes
}
\usage{
rand.index(group1, group2)
adj.rand.index(group1, group2)
}
\arguments{
  \item{group1}{first cluster identity matrix}
  \item{group2}{second cluster identity matrix}
}
\details{
This function calculates the Rand Index for two different clustering outcomes. The Rand Index gives a value between 0 and 1, where 1 means the two clustering outcomes match identicaly. 

The Adjusted Rand Index rescales the index, taking into account that random chance will cause some objects to occupy the same clusters, so the Rand Index will never actually be zero.
}
\value{
a single value between 0 and 1
}
\references{
  Rand, W.M. 1971. Objective criteria for the evaluation of clustering methods. Journal of the American Statistical Association 66: 846--850.
  
  Hubert, L. and Arabie, P. 1985. Comparing partitions. Journal of Classification. 2: 193--218.
}
\author{Matthew Vavrek}

\seealso{To cluster the data, use the function \code{\link{rclust}}}
\examples{
#create a hypothetical clustering outcome with 2 distinct clusters
g1 <- sample(1:2, size=10, replace=TRUE)
g2 <- sample(1:3, size=10, replace=TRUE)
rand.index(g1, g2)
}
