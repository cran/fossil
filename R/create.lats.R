`create.lats` <- 
function(x, loc="location", lat="latitude", long="longitude") {
	b<-c(lat,long)
	d<-which(duplicated(x[,loc])==FALSE)
	lm<-x[d,b]
	if (is.null(dim(lm))==TRUE) lm<-matrix(lm,1,2)
	rownames(lm)<-x[d,loc]
	colnames(lm)<-c("latitude","longitude")
	return(lm)
}

