`create.matrix` <-
function(x, tax.name="genus", locality="locality", time.col=NULL, time=NULL, abund=FALSE, abund.col="abundance")
{
if (is.null(time.col)==FALSE & is.null(time)==FALSE) x<-x[which(x[,time.col]==time),]
a<-tax.name
nr<-length(levels(x[,a]))
rn<-levels(x[,a])
z <- locality
d<-factor(x[,z])
nc<-length(levels(d))
cn<-levels(d)
nm<-matrix(0,nr,nc,dimnames=list(rn,cn))
for (i in 1:length(x[,1])) {
  m<-as.character(x[i,a])
  n<-as.character(x[i,z])
  if (is.na(m)==TRUE | is.null(m)==TRUE | is.na(n)==TRUE | is.null(n)==TRUE) next (i)
  if (m=='' | m==' ' | n=='' | n==' ') next (i)
  if (abund==TRUE) nm[m,n]<-nm[m,n]+x[i,abund.col]
  else nm[m,n]<-1
  }
fm<-nm[rowSums(nm)>0,]
return(fm)
}

