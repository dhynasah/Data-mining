

matrix=read.table("hw09data1.txt", header=TRUE)
my_Kmeans=function(matrix,clustNum) {
  set.seed(200)
  temp=0
  initCenters=initialCentroids(matrix,clustNum)
  org_clust=assignClust(matrix,initCenters)
  updated_cent=initCenters
  while (sum(temp)!=sum(updated_cent)){
    temp=updated_cent
    print(temp)
    updated_clust=assignClust(matrix,updated_cent)
    sse=sse(matrix,temp,updated_clust)
    updated_cent=calcCent(matrix,updated_clust)
    print(sse)
  }
  return(new_centers) 
}

my_Kmeans(matrix,8)
