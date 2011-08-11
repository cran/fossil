\name{sim.occ}
\alias{sim.occ}
\title{Simulated Species Occurrence data}
\description{
  A function to simulate a species occurrence data set
}
\usage{
sim.occ(total.species = 100, endemics = 0.1, regions = 3, locs = 30, avg.abund = 100)
}
\arguments{
  \item{total.species}{The total number of species in the region (i.e. the number of rows in the result matrix)}
  \item{endemics}{The proportion of endemic species for the entire region}
  \item{regions}{The number of areas of endemicity}
  \item{locs}{The number of samples/locatlities per region of endemicity}
  \item{avg.abund}{The 'average' abundance of a species for any given sample}
}
\details{
  The function creates a matrix of $c$ rows of species (given by \code{total.species}) with $n$ number of sample columns (where $n$ equals $regions*locs$). The given abundance of any species at a given sample is determined by a log normal distribution, with each species being randomly assigned a value from \code{rnorm()}. The number of endemics for any given region is equal to $total.species*endemics/regions$. An endemic is conseidered to only occur within a given region, and all other non-ndemic species are considered to be 'cosmopolitan' and can occur in any region. The \code{avg.abund} value affects how many species are recovered at a given site, and for any given run there are typically species that are not present in the sample but are present in the region.
}
\value{
  Returns a matrix of simulated species abundances per locality.
}
\author{Matthew Vavrek}

\seealso{\code{\link{ecol.dist}}}
\examples{
## create a dataset with 2 regions and 5 samples per region
sim.occ(regions=2, locs=5)
}

