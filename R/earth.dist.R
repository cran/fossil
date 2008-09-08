`earth.dist` <-
function(x,dist = TRUE) {
name <- list(rownames(x),rownames(x))
n <- nrow(x)
z <- matrix(,n,n,dimnames=name)
for (i in 1:n) {
for (j in 1:n) z[i,j] <- deg.dist(x[i,1],x[i,2],x[j,1],x[j,2])
}
if (dist==TRUE) z<-as.dist(z)
return(z)
}

