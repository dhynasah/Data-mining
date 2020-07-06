setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw5")
library("e1071")
cData = read.table("hw05c.txt", header =TRUE)
# read data file
aData=read.table("hw05a.txt",header=TRUE)

bData=read.table("hw05b.txt",header=TRUE)
for(i in c(2,3,6)){x[,i]=as.factor(x[,i])}

naiveBayes.default(x= X, y =Y, laplace= laplace)


mod = naiveBayes(virus~.,x[ind-,2:6])

pred = predict(mod,x[ind==0,])

table(pred,x[ind==0,6])
