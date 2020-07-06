#for building perceptron:
wgts=c(.1,.1,.1)  #set initial weights
x=read.table("dataTiny.txt",header=TRUE)
d=ncol(x)
# use cbind to add the column of 1s to the data
# and convert it to a matrix
x1=as.matrix(cbind(1.0,x[,1:d-1]))
# calculate summation of perceptron - predict class for each instance
sign(x1 %*% wgts)
#subtract class column minus prediction and multiply by learning rate
value = .1*(x[,3]-sign(x1 %*% wgts))

#multiply by data
c(value) * x1

#do the colSums and add to the weights
