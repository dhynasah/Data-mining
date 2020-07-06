data=data.matrix(x[ind!=0,])
set.seed(1)
mod = neuralnet(class~var1+var2+var3+var4,data,hidden=3, linear.output=T)
plot(mod)
data2=data.matrix(x[ind==0,])
data2[,3]=data2[,3]*2-3
pred=compute(mod,data2[,1:4])
table(sign(pred$net.result),data2[,5])