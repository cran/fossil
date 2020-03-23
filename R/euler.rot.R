`euler.rot` <-
function(lat1,long1,rotdeg,lat2,long2) {
  rad <- pi/180
  a1<-lat1*rad
  a2<-long1*rad
  rd<-rotdeg*rad
  b1<-lat2*rad
  b2<-long2*rad
  dlon<-b2-a2
  dlat<-b1-a1
  ca1<-cos(a1)
  sa1<-sin(a1)
  cb1<-cos(b1)
  sb1<-sin(b1)
  a<-(sin(dlat/2))^2+ca1*cb1*(sin(dlon/2))^2
  d<-2*atan2(sqrt(a),sqrt(1-a))
  sd<-sin(d)
  cd<-cos(d)
  bear<-atan2(sin(dlon)*cb1,ca1*sb1-sa1*cb1*cos(dlon))
  deg <-(bear%%(2*pi))
  tc<-bear-rd
  nlat<-asin(sa1*cd+ca1*sd*cos(tc))
  ndlon<-atan2(sin(tc)*sd*ca1,cd-sa1*sin(nlat))
  nlon<-((a2+ndlon+pi)%%(2*pi))-pi
  npts<-c(nlat/rad,nlon/rad)
  return(npts)
}