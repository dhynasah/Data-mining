getAvgs = function(x,colNbr){
  
  # assume last column has the class (0/1)
  n=ncol(x)
  ind = x[,n]==0
  ans=c(0,0)
  ans[1]=mean(x[ind,colNbr])
  ans[2]=mean(x[!ind,colNbr])
  return(ans)
}