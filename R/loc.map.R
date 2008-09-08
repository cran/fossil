`loc.map` <-
function(x,...) { 
  xa <- min(x[,2])
  xb <- max(x[,2])
  if ((xb-xa)<(360-(xb-xa))) {
    x1 <- max(xa-5,-180)
    x2 <- min(xb+5,180)
    y1 <- max(min(x[,1])-5,-90)
    y2 <- min(max(x[,1])+5,90)  
    map("world",xlim=c(x1,x2),ylim=c(y1,y2))
    points(x[,2],x[,1],...)
   }
  else {
    nl <- x[,2]
    for (i in 1:length(nl)) {
      if (nl[i]<0) nl[i]<-nl[i]+360}
    x1 <- max(min(nl)-5,0)
    x2 <- min(max(nl)+5,360)
    y1 <- max(min(x[,1])-5,-90)
    y2 <- min(max(x[,1])+5,90)  
    map("world2",xlim=c(x1,x2),ylim=c(y1,y2))
    points(nl,x[,1],...)
  } 
}

