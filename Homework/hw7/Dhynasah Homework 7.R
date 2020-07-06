setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw7")
library(e1071)
library("caret")
y = read.table("hw07dataTest.txt", header =TRUE)
x = read.table("hw07dataTrain.txt", header = TRUE)
var3= x$var3
ind0 = x[,3]==0
ind1 = x[,3]==1
ind2 = x[,3]==2
x01 = x[ind0 | ind1,]
ind = x01[,3]==0
x01[ind,3]=1
x01[!ind,3]=-1
x01[,3]=as.factor(x01[,3])
mod01=svm(x01[,1:2],x01[,3]) #svm only takes numeric data, and also include labels
pred= predict(mod01,y[,1:2])


x02 = x[ind0 | ind2,]
indtwo = x02[,3]==0
x02[indtwo,3]=1
x02[!indtwo,3]=-1
x02[,3]=as.factor(x02[,3])
mod02= svm(x02[,1:2], x02[,3])
pred2 = predict(mod02,y[,1:2])

x12 = x[ind1 | ind2,]
indT = x12[,3]==1
x12[indT,3]=1
x12[!indT,3]=-1
x12[,3]=as.factor(x12[,3])
mod12 = svm(x12[,1:2],x12[,3])
pred3 = predict(mod12, y[,1:2])


pairwise_class = function(pred, pred2, pred3, i){

    if(pred[i]==1 && pred2[i]==1){
      return(0)
    }
    else if(pred2[i]==1 && pred3[i]==1){
      return(1)
    }
    else if(pred[i]==-1 && pred3[i]==-1){
      return(2)
    }
    else
     return(0)
}

finalList = as.character(pred)
for(i in 1:length(pred)){
  predict = pairwise_class(pred,pred2,pred3,i)
  finalList[i]= predict
}


# Needed to convert the actual values to factors to use with confusion matrix
factored_actual = factor(y[,3])
factored_predicted = factor(finalList)

confusionMatrix(factored_predicted,factored_actual)

table(factored_predicted,factored_actual)

