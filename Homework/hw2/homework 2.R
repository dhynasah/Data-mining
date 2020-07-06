setwd("C:/Users/Mauri/Documents/CIS 635/Homework")

x= read.table("hw02data.txt", header = TRUE)
View(x)
v = c(23,41,11,5)
w =c(0,1,1,1)

v[1]
v[2:3]

#multiplicaiton
v*w

#dot product 
v%*%w

#x.read.table('patients1.txt", header=TRUE)
# x[10:15, 2:3] row 10-15 and column 2-3
#mean (x[,1]) mean of column 1 
# when statements  ind=x[,14]==0  so this is when column 14 value is equal to 0 
# mean when people who do not have the disease  mean(x[ind,1])   or mean(x[!ind,1])

#randomly breakup dataset 

sam = sample(10)

# gives you a random set of numbers between 0-3 

sam %% 4
ind = sample(10000) %% 3 
#mean(x[ind==0,1])

a = c(4,7,2,10,5)
b = c(1,0,0,1,1)
multpi = a*b
print(multpi)

a%*%b

x[25,3]
x[1:10,2:3]
colMeans(x[1:2000,2:5])
colMeans()
x[,6] =as.factor(x[,6])
colMeans(x[,2:5])

ind1= sample(nrow(x)) %%3 
View(ind1)
A<- colMeans(x[,2:5],x$passed == "n")
print(A)
