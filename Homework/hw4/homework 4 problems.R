setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw4")
library("caret")
setA = read.table("hw04a.txt", header=TRUE)
setB = read.table("hw04b.txt", header = TRUE)
setC = read.table("hw04c.txt", header = TRUE)

for(i in c(2,3,6)){setA[,i]=as.factor(setA[,i])}
for(i in c(2,3,6)){setB[,i]=as.factor(setB[,i])}
for(i in c(2,3,6)){setC[,i]=as.factor(setC[,i])}

library("rpart.plot")
modA = rpart(virus~.,setA)
modB = rpart(virus~.,setB)
modC = rpart(virus~.,setC)

rpart.plot(modA)

rpart.plot(modB)
rpart.plot(modC)

