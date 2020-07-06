#x[,14] = as.factor(x[,14])
#summary(x)
v = sample(10) #orders the number 1 through 10 randomly 
v
v<4 # is it true for which variables in the vector 
v = sample(nrow(x)) %%3   # 

group1 = v==0 # group 1 is anywhere were v is equal to 0 
ind=x[,14]==0 
plot(x)
plot(x[,1:4])
hist(x[,6], main = "heart rate distribution", xlab = "lbs", col = "green")
boxplot(x[,6])
boxplot(x[ind,6],x[!ind,6], main = "heart reate compare")
boxplot(x[ind,6],x[!ind,6], main = " comparison of heart rate", names=c("disease=1", "disease=0"))
table(x[,13], x[,14])
#[,14] this is for the column 
# plot(x[,1:3], col=x[,4]+1, pch =20)
log.ir = log(x[1:3])

ir.cls = x[,4]
ir.pca = prcomp(log.ir, center = TRUE,scale. = TRUE )
plot(ir.pca$x[,1:2], colx[,4]+1, pch=20)
plus = function plus(x,y){return(x+y)} 
plus(1,5)