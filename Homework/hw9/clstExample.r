clustExample <- function(x,pK){
  t=rep(0,6)
  t[1]=Sys.time()
  km=kmeans(x,pK)
  rtn=list("kmeans"=km$cluster)
  t[2]=Sys.time()
  dis=dist(x)
  t[3]=Sys.time()
  desc=c("single","complete")
  for(i in 1:length(desc)){
    clst=hclust(dis,method=desc[i])
    grps=cutree(clst,k=pK)
    rtn[[desc[i]]]=grps
    t[3+i]=Sys.time()
  }
  minMax=function(v){mx=max(v);mn=min(v);return((v-mn)/(mx-mn))}
  xScaled=x
  for(i in 1:ncol(x)){xScaled[,i]=minMax(x[,i])}
  
  db=dbscan(xScaled,eps=.1,minPts=5)
  rtn[["dbscan"]]=db$cluster
  t[6]=Sys.time()
  rtn[["timer"]]=t[2:6]-t[1:5]
  return(rtn)
}

clstAvgs <- function(x,grps){
  rtn=NULL
  for(i in 1:8){rtn=rbind(rtn,colMeans(x[grps==i,]))}
  return(rtn)
}

saveBarPlot<-function(x,grps,fileName){
  png(fileName)
  barplot(t(clstAvgs(x,grps)),col=c(1,2,3,4,5,6),legend=colnames(x),beside=TRUE)
  dev.off()
}
