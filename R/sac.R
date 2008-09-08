`sac` <-
function(lats,spp) {
tcn <- c(mean(lats[,1]),mean(lats[,2]))
nr<-length(lats[,1])
dtc <- numeric(nr)
for (i in 1:nr) dtc[i]<-abs(tcn[1]-lats[i,1])+abs(tcn[2]-lats[i,2])
rcn <- which.min(dtc)
v<-earth.dist(lats,FALSE)[rcn,]
id<-names(v[rcn])
rv<-as.vector(rank(v))
sac<-matrix(,(nr-2),2)
for (i in 3:nr) {
q<-which(rv<=i)
sac[(i-2),1]<-earth.poly(lats[q,],FALSE)
w<-rowSums(spp[,q])
e<-length(w[w>0])
sac[(i-2),2]<-e
}
attr(sac,"point order") <- rank(v)
return(sac)
}

