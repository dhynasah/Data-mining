#install and include the e1071 package
library("e1071", lib.loc="~/u/R/x86_64-pc-linux-gnu-library/3.3")

# read data file
x=read.table("hw05a.txt",header=TRUE)

# convert variables to factors as necessary
for(i in c(2,3,6)){x[,i]=as.factor(x[,i])}

# stratify data
ind = sample(nrow(x)) %% 10
# we will assume the 0's are the test records 

# build the model
mod=naiveBayes(virus~.,x[ind!=0,2:6])

# look at the model
mod

# predict the test records
pred = predict(mod,x[ind==0,])

table(pred,x[ind==0,6])
