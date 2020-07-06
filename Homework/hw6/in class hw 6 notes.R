weights = c(.1,.1,.1)
d = ncol(x)
x1 = as.matrix(x(cbind(1.0,x[,1:d-1])))  #add a column of 1's to the data set
#calculate summation of percepton
sign(x1 %% weights)

#subtract calss colmn minus summation
value = .1*(x[,3]-sign((x1%% weights)))
weights + colSums(x1*c(value))

wgts = f(...)
nerualnet()