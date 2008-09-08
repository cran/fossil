`lats2Shape` <-
function(x) { 
  n<-dim(x)[1]
a<-data.frame(Id=1:n,X=x[,2],Y=x[,1])    
  aa<-data.frame(Id=1:n,locality=row.names(as.data.frame(x)),x)
  ashp<-convert.to.shapefile(a,aa,"Id",1)
  return(ashp)
}

