setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw6")
install.packages("neuralnet")
library("neuralnet")
install.packages("caret")
library("caret")
pW = c(.1,.1,.1)
updateWeights <- function(data, pW, learnRate){
  
  d = ncol(data)
  dataMatrix = as.matrix(cbind(1.0,data[,1:d-1]))
  predClass = sign(dataMartrix %% pW)
  value = learnRate*(data[,3]-predClass)
  weight = pw + colSums(dataMatrix*c(value))
  return(weight)
}
data = read.table("hw06dataTrain.txt",header=TRUE)
preceptron <- function(data, pW,updateWeights){
  previousweight = sum(abs(updateWeights))
   while(currentweight!=previousweight){
     previousweight = sum(abs(currentweight))
    currentweight = updateWeights(data, pW, learnRate)
    currentweight = sum(abs(currentweight))
   }
  return(currentweight)
}
print(preceptron(data = data, pW, learnRate= .1))
data = read.table("hw06dataTrain.txt",header=TRUE)
class <- data$class
var1 <- data$var1
var2<- data$var2
var3<- data$var3
var4<- data$var4


data=data.matrix(x[ind!=0,])
set.seed(1)
mod = neuralnet(class~var1+var2+var3+var4,data,hidden=3, linear.output=T)
plot(mod)
data2=data.matrix(x[ind==0,])
data2[,3]=data2[,3]*2-3
pred=compute(mod,data2[,1:4])
table(sign(pred$net.result),data2[,5])


class = as.factor(data$class)
ind = class==0
matrix = data.matrix(data[ind!=0,])
set.seed(1)
nn<-neuralnet(class == "class"~var1+var2+var3+var4,data,hidden=3, linear.output = T)
plot(nn)
dataMat = data.matrix(data[ind==0,])
dataMat[,3]= dataMat[,3]*2-3
pred = compute(nn,dataMat[,1:4])
table(sign(pred$net.result),dataMat[,5])
