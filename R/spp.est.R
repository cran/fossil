`spp.est` <-
function(x, rand = 10, abund = TRUE, counter = TRUE) {
  if (abund==TRUE) {
    if (ncol(as.matrix(x))>1 & nrow(as.matrix(x))>1) x<-rowSums(x)
    if (max(x)==1) print("cannot use incidence data for abundance-based analyses")
    n<-length(x)
    m<-sum(x)
    ests<-list(chao1,chao.sd,ACE,jack1)
    cn<-c("N.obs", "S.obs", "S.obs(+95%)", "S.obs(-95%)", "Chao1", "Chao1(upper)", "Chao1(lower)", "ACE", "ACE(upper)", "ACE(lower)", "Jack1", "Jack1(upper)", "Jack1(lower)")
  }
  else {
    x[x>1]<-1
    n<-ncol(x)
    m<-n
    ests<-list(chao2,chao.sd,ICE,jack1)
    cn<- c("# Samples", "S.obs", "S.obs(+95%)", "S.obs(-95%)", "Chao2", "Chao2(upper)", "Chao2(lower)", "ICE", "ICE(lower)", "ICE(lower)", "Jack1", "Jack1(lupper)", "Jack1(lower)")
  }
  
##se is final output table, see n below
se<-matrix(,m,13)
le<-length(ests)
##if x is a vector
if (abund==TRUE) ss<-rep(1:n,x)
else ss<-1:n
for (i in 1:m) {
	avg<-matrix(,rand,le+1)
	for (j in 1:rand) {
		ssc<-sample(ss,i,FALSE)
		if (abund==TRUE) {
      b<-numeric(n)
      for(k in 1:i) b[ssc[k]]<-b[ssc[k]]+1
    }

		else {if (i>1) b<-rowSums(x[,ssc])
      else b<-x[,ssc]
    }
##run all stats at this point
		avg[j,1]<-length(b[b>0])

		for (k in 1:le) avg[j,(k+1)]<-ests[[k]](b)
	}

  se[,1]<-1:m ##number of samples taken
	se[i,2]<-mean(avg[,1]) ##sobs
	se[i,3]<-se[i,2]+1.96*(sd(avg[,1])) ##sobs + stdev
	se[i,4]<-2*se[i,2]-se[i,3] ##sobs - stdev
	se[i,5]<-mean(avg[,2]) ##mean chao
	se[i,6]<-se[i,5]-mean(avg[,3],na.rm=TRUE) ##chao-mean chaosd
	se[i,7]<-2*se[i,5]-se[i,6] ##chao-mean chaosd
	
	## check to see if any numers are real
  re<-avg[,4][avg[,4]<Inf & is.na(avg[,4])==FALSE]
	se[i,8]<-mean(avg[,4][avg[,4]<Inf],na.rm=TRUE)##mean ACE
	if (length(re)>0) se[i,9] <- se[i,8]+1.96*(sd(avg[,4][avg[,4]<Inf],na.rm=TRUE))  ##ACE+acesd
	se[i,10]<-2*se[i,8]-se[i,9]   ##ACE-acesd
	
	## check to see if any numbers are real
  re<-avg[,5][avg[,5]<Inf & is.na(avg[,5])==FALSE]
	se[i,11]<-mean(avg[,5][avg[,5]<Inf],na.rm=TRUE)##mean jack
	if (length(re)>0) se[i,12]<-se[i,11]+1.96*(sd(avg[,5][avg[,5]<Inf],na.rm=TRUE))  ##jack+jacksd
	se[i,13]<-2*se[i,11]-se[i,12]   ##jack-jacksd
	
	##etc  more to be added
	if (counter==TRUE) print(paste(i, "out of", m, "runs"))
}
if (abund==TRUE) attr(se,"data.type")<-"abundance"
else attr(se,"data.type")<-"presence\absence"
colnames(se)<-cn
return(se)
}


