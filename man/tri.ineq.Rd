\name{tri.ineq}
\alias{tri.ineq}
\title{Testing for the Triangle Inequality}
\description{
  Determines if a distance matrix obeys the triangle inequality
}
\usage{
tri.ineq(dist)
}
\arguments{
  \item{dist}{A distance matrix}
}
\details{
  Tests if a distance matrix respects the triangle inequality. Often with non-monotonic distance measures and complex data a situation can arise where the triangle inequality (where no single side of a triangle is greater in length than the sum of the other two sides) is not respected.
}
\value{
  Returns a TRUE if the inequality is respected, and a FALSE if there is any situation where the triangle inequality is not respected.
}
\author{Matthew Vavrek}

\seealso{\code{\link{ecol.dist}}}
\examples{
## sample distance matrix with an impossible triangle
a<-matrix(0.2, 4,4)
a[4,2]<-0.8
a<-as.dist(a)
tri.ineq(a)
}

