setwd("C:/Users/Mauri/Documents/CIS 635/Lecture slides")
x =read.table("patients1.txt", header=TRUE)
View(x)

#change class factors/ nominal 
x[,14] = as.factor(x[,14])

#use a loop to change many attributes
for(i in 7:14){x[,i]=as.factor(x[,i])}

library("rpart.plot")


#build a model(formula, data)

mod = rpart(disease~.,x)  #the dot means include everthing

modtry = rpart(disease~age+weight+height+heartRate,x)

#inspect the tree
mod
# the stars tell you when you got to a leaf of the tree 
# a better way to see the tree

rpart.plot(mod)

#prediction
sample1 = sample(nrow(x)) %% 3 #create a vector of random groups 
mod2 = rpart(disease~.,x[sample1<2,]) #build model using groups 0 and 1 
pred = predict(mod2, x[sample1==2,],type ="vector") #make predicton using sample 2

pred = predict(mod2, x[sample1==2,],type ="vector")-1

#first 10 predictions 
pred[1:10]

# actual values 
act=x[sample1==2,14]
act[1:10]

table(pred, act)
