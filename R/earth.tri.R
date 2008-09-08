`earth.tri` <-
function(lat1,long1,lat2,long2,lat3,long3) {
war<-"Warning: site 1 and 2 are exactly opposite one another, and so the triangle is undefined"
if (((long1-long2)^2-(lat1-lat2))==32400) print(war)
if (((long1-long3)^2-(lat1-lat3))==32400) print(war)
if (((long2-long3)^2-(lat2-lat3))==32400) print(war)
R <- 40003/(2*pi)
cx<-deg.dist(lat1,long1,lat2,long2)/R
bx<-deg.dist(lat1,long1,lat3,long3)/R
ax<-deg.dist(lat2,long2,lat3,long3)/R
A<-acos((cos(ax)-cos(bx)*cos(cx))/(sin(bx)*sin(cx)))
B<-acos((cos(bx)-cos(cx)*cos(ax))/(sin(cx)*sin(ax)))
C<-acos((cos(cx)-cos(ax)*cos(bx))/(sin(ax)*sin(bx)))
SA<-R^2*((A+B+C)-pi)
return(SA)
}

