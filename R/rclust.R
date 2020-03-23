`rclust` <- 
function(dist, clusters = 2, runs = 10, counter = FALSE) { 
  if (runs == 1) return(relational.clustering(dist, clusters))
  else {
    rc2return <- NULL
    rc2 <- sum(dist) 
    for (i in 1:runs) {
      npart <- relational.clustering(dist, clusters)
      if (i%%10==0 && counter==TRUE) print(paste('Calculating run number ',i,sep=''))
      if (length(npart) == 1) {
        next(i)
      }
      nd <- as.matrix(dist)
      n<-dim(nd)[1]
      nd[upper.tri(nd, diag = TRUE)] <- 0
      inout <- matrix(0,n,n)
      inout <- outer(1:n, 1:n, function(i, j) npart[i]!=npart[j])
      inout<-as.matrix(as.dist(inout))
      sumin <- sum(nd[which(inout==1)])
      sumn<-sum(1:(n-1))
      np <- sumin/(sumn-sum(inout)) 
      if (np < rc2) {
        rc2return <- npart
        rc2 <- np
      }
    }
  }
  return(rc2return)
}



