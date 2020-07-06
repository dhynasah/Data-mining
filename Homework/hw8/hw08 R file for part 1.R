a=read.table("hw08 example1.txt",header=TRUE)
b=read.table("hw08 example2.txt",header=TRUE)
c=read.table("hw08 example4.txt",header=TRUE)
d=read.table("hw08 example5.txt",header=TRUE)

library(plyr)
form="class~var1+var2"
foldsa=split(a, cut(sample(1:nrow(a)),10))
foldsb=split(b, cut(sample(1:nrow(b)),10))
foldsc=split(c, cut(sample(1:nrow(c)),10))
foldsd=split(d, cut(sample(1:nrow(d)),10))

#Decision Tree
library(rpart)
for (i in 1:length(foldsa)) {
  testa=ldply(foldsa[i], data.frame)
  traina=ldply(foldsa[-i], data.frame)
  dtmodela=rpart(form,traina,method = "class")
  dtpredicta=predict(dtmodela,testa,type="class")
  dtcma=table(testa$class,dtpredicta)
  accuracya=100*sum(diag(dtcma))/sum(dtcma)
  print(paste("A",accuracya,"%"))
  precision=100*dtcma[1,1]/sum(dtcma[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*dtcma[1,1]/sum(dtcma[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=90.9%
#AvgP=77.2%
#AvgR=85.7%

for (i in 1:length(foldsb)) {
  testb=ldply(foldsb[i], data.frame)
  trainb=ldply(foldsb[-i], data.frame)
  dtmodelb=rpart(form,trainb,method = "class")
  dtpredictb=predict(dtmodelb,testb,type="class")
  dtcmb=table(testb$class,dtpredictb)
  accuracyb=100*sum(diag(dtcmb))/sum(dtcmb)
  print(paste("A",accuracyb,"%"))
  precision=100*dtcmb[1,1]/sum(dtcmb[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*dtcmb[1,1]/sum(dtcmb[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=82.6%
#AvgP=75.8%
#AvgR=87.0%

for (i in 1:length(foldsc)) {
  testc=ldply(foldsc[i], data.frame)
  trainc=ldply(foldsc[-i], data.frame)
  dtmodelc=rpart(form,trainc,method = "class")
  dtpredictc=predict(dtmodelc,testc,type="class")
  dtcmc=table(testc$class,dtpredictc)
  accuracyc=100*sum(diag(dtcmc))/sum(dtcmc)
  print(paste("A",accuracyc,"%"))
  precision=100*dtcmc[1,1]/sum(dtcmc[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*dtcmc[1,1]/sum(dtcmc[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=90.1%
#AvgP=89.9%
#AvgR=90.2%

for (i in 1:length(foldsd)) {
  testd=ldply(foldsd[i], data.frame)
  traind=ldply(foldsd[-i], data.frame)
  dtmodeld=rpart(form,traind,method = "class")
  dtpredictd=predict(dtmodeld,testd,type="class")
  dtcmd=table(testd$class,dtpredictd)
  accuracyd=100*sum(diag(dtcmd))/sum(dtcmd)
  print(paste("A",accuracyd,"%"))
  precision=100*dtcmd[1,1]/sum(dtcmd[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*dtcmd[1,1]/sum(dtcmd[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=93.9%
#AvgP=93.9%
#AvgR=93.8%

#Naive Bayes
library(e1071)
for (i in 1:length(foldsa)) {
  testa=ldply(foldsa[i],data.frame)
  traina=ldply(foldsa[-i],data.frame)
  g=(traina[,2:4])
  h=(testa[,2:4])
  g[,3]=as.factor(g[,3])
  h[,3]=as.factor(h[,3])
  nbmodela=naiveBayes(class~.,g)
  nbpredicta=predict(nbmodela,h)
  nbcma=table(nbpredicta,h[,3])
  accuracya=100*sum(diag(nbcma))/sum(nbcma)
  print(paste("A",accuracya,"%"))
  precision=100*nbcma[1,1]/sum(nbcma[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nbcma[1,1]/sum(nbcma[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=74.5%
#AvgP=NaN%
#AvgR=0%

for (i in 1:length(foldsb)) {
  testb=ldply(foldsb[i],data.frame)
  trainb=ldply(foldsb[-i],data.frame)
  g=(trainb[,2:4])
  h=(testb[,2:4])
  g[,3]=as.factor(g[,3])
  h[,3]=as.factor(h[,3])
  nbmodelb=naiveBayes(class~.,g)
  nbpredictb=predict(nbmodelb,h)
  nbcmb=table(nbpredictb,h[,3])
  accuracyb=100*sum(diag(nbcmb))/sum(nbcmb)
  print(paste("A",accuracyb,"%"))
  precision=100*nbcmb[1,1]/sum(nbcmb[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nbcmb[1,1]/sum(nbcmb[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=83.6%
#AvgP=84.8%
#AvgR=81.2%

for (i in 1:length(foldsc)) {
  testc=ldply(foldsc[i],data.frame)
  trainc=ldply(foldsc[-i],data.frame)
  g=(trainc[,2:4])
  h=(testc[,2:4])
  g[,3]=as.factor(g[,3])
  h[,3]=as.factor(h[,3])
  nbmodelc=naiveBayes(class~.,g)
  nbpredictc=predict(nbmodelc,h)
  nbcmc=table(nbpredictc,h[,3])
  accuracyc=100*sum(diag(nbcmc))/sum(nbcmc)
  print(paste("A",accuracyc,"%"))
  precision=100*nbcmc[1,1]/sum(nbcmc[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nbcmc[1,1]/sum(nbcmc[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=79.8%
#AvgP=79.6%
#AvgR=79.9%

for (i in 1:length(foldsd)) {
  testd=ldply(foldsd[i],data.frame)
  traind=ldply(foldsd[-i],data.frame)
  g=(traind[,2:4])
  h=(testd[,2:4])
  g[,3]=as.factor(g[,3])
  h[,3]=as.factor(h[,3])
  nbmodeld=naiveBayes(class~.,g)
  nbpredictd=predict(nbmodeld,h)
  nbcmd=table(nbpredictd,h[,3])
  accuracyd=100*sum(diag(nbcmd))/sum(nbcmd)
  print(paste("A",accuracyd,"%"))
  precision=100*nbcmd[1,1]/sum(nbcmd[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nbcmd[1,1]/sum(nbcmd[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=65.2%
#AvgP=65.6%
#AvgR=63.6%

#ANN
library(nnet)
for (i in 1:length(foldsa)) {
  testa=ldply(foldsa[i])
  traina=ldply(foldsa[-i])
  k=(traina[,2:4])
  l=(testa[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  nnmodela=nnet(class~.,k,size=4,trace=FALSE)
  nnpredicta=predict(nnmodela,l,type="class")
  nn_cma=table(l$class,nnpredicta)
  accuracy_a=100*sum(diag(nn_cma))/sum(nn_cma)
  print(paste("A",accuracy_a,"%"))
  precision=100*nn_cma[1,1]/sum(nn_cma[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nn_cma[1,1]/sum(nn_cma[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=86.6%
#AvgP=67.4%
#AvgR=79.5%

for (i in 1:length(foldsb)) {
  testb=ldply(foldsb[i])
  trainb=ldply(foldsb[-i])
  k=(trainb[,2:4])
  l=(testb[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  nnmodelb=nnet(class~.,k,size=4,trace=FALSE)
  nnpredictb=predict(nnmodelb,l,type="class")
  nn_cmb=table(l$class,nnpredictb)
  accuracy_b=100*sum(diag(nn_cmb))/sum(nn_cmb)
  print(paste("A",accuracy_b,"%"))
  precision=100*nn_cmb[1,1]/sum(nn_cmb[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nn_cmb[1,1]/sum(nn_cmb[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=79.3% (jpt)
#AvgP=85.8% (jpt)
#AvgR=73.5% (jpt)

for (i in 1:length(foldsc)) {
  testc=ldply(foldsc[i])
  trainc=ldply(foldsc[-i])
  k=(trainc[,2:4])
  l=(testc[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  nnmodelc=nnet(class~.,k,size=4,trace=FALSE)
  nnpredictc=predict(nnmodelc,l,type="class")
  nn_cmc=table(l$class,nnpredictc)
  accuracy_c=100*sum(diag(nn_cmc))/sum(nn_cmc)
  print(paste("A",accuracy_c,"%"))
  precision=100*nn_cmc[1,1]/sum(nn_cmc[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nn_cmc[1,1]/sum(nn_cmc[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=54.7% (jpt)
#AvgP=66.9% (jpt)
#AvgR=62.4% (jpt)

for (i in 1:length(foldsd)) {
  testd=ldply(foldsd[i])
  traind=ldply(foldsd[-i])
  k=(traind[,2:4])
  l=(testd[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  nnmodeld=nnet(class~.,k,size=4,trace=FALSE)
  nnpredictd=predict(nnmodeld,l,type="class")
  nn_cmd=table(l$class,nnpredictd)
  accuracy_d=100*sum(diag(nn_cmd))/sum(nn_cmd)
  print(paste("A",accuracy_d,"%"))
  precision=100*nn_cmd[1,1]/sum(nn_cmd[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*nn_cmd[1,1]/sum(nn_cmd[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=85.5%
#AvgP=85.4%
#AvgR=85.4%

#SVM
library(e1071)
for (i in 1:length(foldsa)) {
  testa=ldply(foldsa[i])
  traina=ldply(foldsa[-i])
  k=(traina[,2:4])
  l=(testa[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  svmodela=svm(class~.,k)
  svpredicta=predict(svmodela,l[,1:2])
  sv_cma=table(l[,3],svpredicta)
  accuracy_a=100*sum(diag(sv_cma))/sum(sv_cma)
  print(paste("A",accuracy_a,"%"))
  precision=100*sv_cma[1,1]/sum(sv_cma[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*sv_cma[1,1]/sum(sv_cma[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=89.3%
#AvgP=84.5%
#AvgR=76.3%

for (i in 1:length(foldsb)) {
  testb=ldply(foldsb[i])
  trainb=ldply(foldsb[-i])
  k=(trainb[,2:4])
  l=(testb[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  svmodelb=svm(class~.,k)
  svpredictb=predict(svmodelb,l[,1:2])
  sv_cmb=table(l[,3],svpredictb)
  accuracy_b=100*sum(diag(sv_cmb))/sum(sv_cmb)
  print(paste("A",accuracy_b,"%"))
  precision=100*sv_cmb[1,1]/sum(sv_cmb[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*sv_cmb[1,1]/sum(sv_cmb[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=83.4%
#AvgP=77.7%
#AvgR=87.0%

for (i in 1:length(foldsc)) {
  testc=ldply(foldsc[i])
  trainc=ldply(foldsc[-i])
  k=(trainc[,2:4])
  l=(testc[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  svmodelc=svm(class~.,k)
  svpredictc=predict(svmodelc,l[,1:2])
  sv_cmc=table(l[,3],svpredictc)
  accuracy_c=100*sum(diag(sv_cmc))/sum(sv_cmc)
  print(paste("A",accuracy_c,"%"))
  precision=100*sv_cmc[1,1]/sum(sv_cmc[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*sv_cmc[1,1]/sum(sv_cmc[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=94.2%
#AvgP=94.4%
#AvgR=93.9%

for (i in 1:length(foldsd)) {
  testd=ldply(foldsd[i])
  traind=ldply(foldsd[-i])
  k=(traind[,2:4])
  l=(testd[,2:4])
  k[,3]=as.factor(k[,3])
  l[,3]=as.factor(l[,3])
  svmodeld=svm(class~.,k)
  svpredictd=predict(svmodeld,l[,1:2])
  sv_cmd=table(l[,3],svpredictd)
  accuracy_d=100*sum(diag(sv_cmd))/sum(sv_cmd)
  print(paste("A",accuracy_d,"%"))
  precision=100*sv_cmd[1,1]/sum(sv_cmd[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*sv_cmd[1,1]/sum(sv_cmd[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=95.7%
#AvgP=95.7%
#AvgR=95.6%

#KNN
library(class)
for (i in 1:length(foldsa)) {
  testa=ldply(foldsa[i])
  traina=ldply(foldsa[-i])
  m=(traina[,2:4])
  n=(testa[,2:4])
  m[,3]=as.factor(m[,3])
  n[,3]=as.factor(n[,3])
  knnmodela=knn(m,n,cl=m[,3],k=3)
  knn_cma=table(knnmodela,n[,3])
  accuracy_a=100*sum(diag(knn_cma))/sum(knn_cma)
  print(paste("A",accuracy_a,"%"))
  precision=100*knn_cma[1,1]/sum(knn_cma[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*knn_cma[1,1]/sum(knn_cma[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=90.3%
#AvgP=82.0%
#AvgR=79.2%

for (i in 1:length(foldsb)) {
  testb=ldply(foldsb[i])
  trainb=ldply(foldsb[-i])
  m=(trainb[,2:4])
  n=(testb[,2:4])
  m[,3]=as.factor(m[,3])
  n[,3]=as.factor(n[,3])
  knnmodelb=knn(m,n,cl=m[,3],k=3)
  knn_cmb=table(knnmodelb,n[,3])
  accuracy_b=100*sum(diag(knn_cmb))/sum(knn_cmb)
  print(paste("A",accuracy_b,"%"))
  precision=100*knn_cmb[1,1]/sum(knn_cmb[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*knn_cmb[1,1]/sum(knn_cmb[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=94.9%
#AvgP=94.8%
#AvgR=94.7%

for (i in 1:length(foldsc)) {
  testc=ldply(foldsc[i])
  trainc=ldply(foldsc[-i])
  m=(trainc[,2:4])
  n=(testc[,2:4])
  m[,3]=as.factor(m[,3])
  n[,3]=as.factor(n[,3])
  knnmodelc=knn(m,n,cl=m[,3],k=3)
  knn_cmc=table(knnmodelc,n[,3])
  accuracy_c=100*sum(diag(knn_cmc))/sum(knn_cmc)
  print(paste("A",accuracy_c,"%"))
  precision=100*knn_cmc[1,1]/sum(knn_cmc[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*knn_cmc[1,1]/sum(knn_cmc[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=93.8%
#AvgP=93.8%
#AvgR=93.9%

for (i in 1:length(foldsd)) {
  testd=ldply(foldsd[i])
  traind=ldply(foldsd[-i])
  m=(traind[,2:4])
  n=(testd[,2:4])
  m[,3]=as.factor(m[,3])
  n[,3]=as.factor(n[,3])
  knnmodeld=knn(m,n,cl=m[,3],k=3)
  knn_cmd=table(knnmodeld,n[,3])
  accuracy_d=100*sum(diag(knn_cmd))/sum(knn_cmd)
  print(paste("A",accuracy_d,"%"))
  precision=100*knn_cmd[1,1]/sum(knn_cmd[1,1:2])
  print(paste("P",precision,"%"))
  recall=100*knn_cmd[1,1]/sum(knn_cmd[1:2,1])
  print(paste("R",recall,"%"))
}
#AvgA=94.9%
#AvgP=94.9%
#AvgR=94.9%
