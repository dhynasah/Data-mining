#homework 8 
#Dhynasah Cakir 3/29/2019

#precision 
#recall
#accuracy 

setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw8")
library("rpart.plot")
library("caret")
library("e1071")
library(plyr)
library(nnet)
library(class)
atable = read.table("example1.txt", header = TRUE)
btable = read.table("example2.txt", header = TRUE)
ctable = read.table("example4.txt", header = TRUE)
dtable = read.table("example5.txt", header = TRUE)



#  decision tree
decision_tree = function(table) {
  class = as.factor(table[,3])
  ind0 = table[,3]==0
  ind1 = table[,3]==1
  modAtab = rpart(class~.,table[1:5000,], method = 'class')
  test = table[1:5000,]
  actual = table[5001:10000,]
  a_pred = predict(modAtab,test, type ="class")
  confMatrix <- table(test$class,a_pred)
  d_tree <- data.frame(accuracy = c(0), recall = c(0), precision = c(0))
  d_tree[1]  <- c(100*(sum(diag(confMatrix))/ sum(confMatrix)))   # accuracy
  d_tree[2]<- 100*(confMatrix[1]/sum(confMatrix[1:2]))     # recall
  d_tree[3] <- 100*(confMatrix[1]/sum(confMatrix[1] + confMatrix[3])) # precision
  d_tree_stats =as.matrix(d_tree)
  print(d_tree_stats)
}




#niave bayes 
niave_bayes = function(table){
  dfs =split(table, cut(sample(1:nrow(table)),10))
  for (i in 1:length(dfs)) {
    testa=ldply(dfs[i],data.frame)
    traina=ldply(dfs[-i],data.frame)
  }
  x=(traina[,2:4])
  y=(testa[,2:4])
  x[,3]=as.factor(x[,3])
  y[,3]=as.factor(y[,3])
  niaveB_model=naiveBayes(class~.,x)
  nb_pred=predict(niaveB_model,y)
  nb_CfMatrix = table(nb_pred,y[,3])
  nb_stats_list <- data.frame(accuracy = c(0), recall = c(0), precision = c(0))
  nb_stats_list[1]=100*sum(diag(nb_CfMatrix))/sum(nb_CfMatrix)
  nb_stats_list[2]=100*nb_CfMatrix[1,1]/sum(nb_CfMatrix[1,1:2])
  nb_stats_list[3]=100*nb_CfMatrix[1,1]/sum(nb_CfMatrix[1:2,1])
  nb_stats = as.matrix(nb_stats_list)
  print(nb_stats)
}  




#Artifical Neural Net 
Ann = function(table){
  dfs =split(table, cut(sample(1:nrow(table)),10))
  for (i in 1:length(dfs)) {
   test=ldply(dfs[i],data.frame)
   train=ldply(dfs[-i],data.frame)
  }
  set1=(train[,2:4])
  set2=(test[,2:4])
  set1[,3]=as.factor(set1[,3])
  set2[,3]=as.factor(set2[,3])
  nn_model=nnet(class~.,set1,size=4,trace=FALSE)
  nn_pred =predict(nn_model,set2,type="class")
  nn_ConfMatrix=table(set2$class,nn_pred)
  nn_stats_list <- data.frame(accuracy = c(0), recall = c(0), precision = c(0))
  nn_stats_list[1]=100*sum(diag(nn_ConfMatrix))/sum(nn_ConfMatrix)
  nn_stats_list[2]=100*nn_ConfMatrix[1,1]/sum(nn_ConfMatrix[1,1:2])
  nn_stats_list[3]=100*nn_ConfMatrix[1,1]/sum(nn_ConfMatrix[1:2,1])
  nn_stats = as.matrix(nn_stats_list)
  print(nn_stats)
}
  
  

  
SVM_function = function(table){
  dfs =split(table, cut(sample(1:nrow(table)),10))
  for (i in 1:length(dfs)) {
    test=ldply(dfs[i],data.frame)
    train=ldply(dfs[-i],data.frame)
  }
  set1=(train[,2:4])
  set2=(test[,2:4])
  set1[,3]=as.factor(set1[,3])
  set2[,3]=as.factor(set2[,3])
  svm_model=svm(class~.,set1)
  svm_pred=predict(svm_model,set2[,1:2])
  svm_confM=table(set2[,3],svm_pred)
  svm_stats_list <- data.frame(accuracy = c(0), recall = c(0), precision = c(0))
  svm_stats_list[1]=100*sum(diag(svm_confM))/sum(svm_confM)
  svm_stats_list[2]=100*svm_confM[1,1]/sum(svm_confM[1,1:2])
  svm_stats_list[3]=100*svm_confM[1,1]/sum(svm_confM[1:2,1])
  svm_stats = as.matrix(svm_stats_list)
  print(svm_stats)
}




#k nearest neighbor 

knn_fun <- function(table){
  dfs =split(table, cut(sample(1:nrow(table)),10))
  for (i in 1:length(dfs)) {
    test=ldply(dfs[i],data.frame)
    train=ldply(dfs[-i],data.frame)
  }
  set1=(train[,2:4])
  set2=(test[,2:4])
  set1[,3]=as.factor(set1[,3])
  set2[,3]=as.factor(set2[,3])
  knn_model = knn(set1, set2, cl= set1[,3], k= 3)
  knn_confM=table(set2[,3],knn_model)
  knn_stats_list <- data.frame(accuracy = c(0), recall = c(0), precision = c(0))
  knn_stats_list[1]=100*sum(diag(knn_confM))/sum(knn_confM)
  knn_stats_list[2]=100*knn_confM[1,1]/sum(knn_confM[1,1:2])
  knn_stats_list[3]=100*knn_confM[1,1]/sum(knn_confM[1:2,1])
  knn_stats = as.matrix(knn_stats_list)
  print(knn_stats)
  
}

decision_tree(atable)
niave_bayes(atable)
Ann(atable)
SVM_function(atable)
knn_fun(dtable)
knn_fun(ctable)

decision_tree(btable)
niave_bayes(btable)
Ann(btable)
SVM_function(btable)



decision_tree(ctable)
niave_bayes(ctable)
Ann(ctable)
SVM_function(ctable)


decision_tree(dtable)
niave_bayes(dtable)
Ann(dtable)
SVM_function(dtable)


add_column = function(ctable){
   newcolumn = floor(runif(10000,min = 1, max = 101))
  ctable$newcolumn = cbind(newcolumn)
}


calc_stats = function(ctable){
  for(i in 1:20){
    add_column(ctable)
  }
  
    decision_tree(ctable)
    #naiveBayes(ctable)
    #knn(ctable)
}




#part 2
list = c("a","b","c","d","e","f","g","h","j","k","l","m","n","o","p","q","r","s","t","u")


  s= floor(runif(10000,min =1, max = 101))
  ctable$d = cbind(d)
  ctable$e = cbind(e)
  ctable$f = cbind(f)
  ctable$g = cbind(g)
  ctable$h = cbind(h)
  ctable$i = cbind(i)
  ctable$l = cbind(l)
  ctable$n =cbind(n)
  ctable$newcolumn = cbind(newcolumn)
  ctable$o = cbind(o)
  ctable$p = cbind(p)
  ctable$q = cbind(q)
  ctable$r =cbind(r)
  ctable$s = cbind(s)
  
  

calc_stats(ctable)
decision_tree(ctable)
naiveBayes(ctable)
knn(ctable)


dfs =split(ctable, cut(sample(1:nrow(ctable)),10))
for (i in 1:length(dfs)) {
  test=ldply(dfs[i],data.frame)
  train=ldply(dfs[-i],data.frame)
}
x=(train[,2:ncol(ctable)])
y=(test[,2:ncol(ctable)])
x[,3]=as.factor(x[,3])
y[,3]=as.factor(y[,3])
niaveB_model=naiveBayes(class~.,x)
nb_pred=predict(niaveB_model,y)
nb_CfMatrix = table(nb_pred,y[,3])
nb_stats_list <- data.frame(accuracy = c(0), recall = c(0), precision = c(0))
nb_stats_list[1]=100*sum(diag(nb_CfMatrix))/sum(nb_CfMatrix)
nb_stats_list[2]=100*nb_CfMatrix[1,1]/sum(nb_CfMatrix[1,1:2])
nb_stats_list[3]=100*nb_CfMatrix[1,1]/sum(nb_CfMatrix[1:2,1])
nb_stats = as.matrix(nb_stats_list)
print(nb_stats)
