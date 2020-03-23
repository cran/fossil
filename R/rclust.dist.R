rclust.dist <- 
function(groups, dist) {
  c<-length(table(groups))
  wts <- rclust.weights(groups, dist)
  finals<-NULL
  for (i in 1:c) {
    grp.ids<-which(groups==i)
    finals<-rbind(finals,colSums(wts[grp.ids,])/length(grp.ids))
  }
  colnames(finals)<-levels(as.factor(groups))
  rownames(finals)<-levels(as.factor(groups))
  return(finals)
}
