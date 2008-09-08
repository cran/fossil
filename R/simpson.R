`simpson` <-
function(x,y) 
{
ai<-(y[y>0]-x[y>0])!=y[y>0]
a<-length(ai[ai==TRUE]) 
b<-length(x[x>0])
c<-length(y[y>0])
simp <- a/min(b,c)
return(simp)
}

