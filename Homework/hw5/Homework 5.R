setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw5")

virusdata<- data.frame(id = c("101","102","103","104","105","106", "107","108","109","110"),
                 gender = c("M","M","F","M","F","F","M","F","M","F"),
                 AgeGroup =c(1,3,2,2,1,3,1,2,3,2),
                 pulse = c(67,68,73,77,62,81,70,81,105,63),
                 test =c(45,58,72,67,39,61,73,52,66,47),
                 virus = c("Y","N","N","N","Y","N","N","Y","N","Y"))
aggregate(virusdata$pulse, list(virusdata$virus), sd)
aggregate(virusdata$test, list(virusdata$virus), sd)
#yes
dnorm(66, mean = 75, sd=8)
dnorm(90, mean = 75, sd=8)
dnorm(50, mean = 52, sd=12)
dnorm(81, mean = 52, sd=12)
dnorm(55, mean = 52, sd=12)

#no
dnorm(50, mean = 62, sd=7)
dnorm(81, mean = 62, sd=7)
dnorm(55, mean = 62, sd=7)

dnorm(50, mean = 48, sd=11)
dnorm(81, mean = 48, sd=11)
dnorm(55, mean = 48, sd=11)


#part 3 
cData = read.table("hw05c.txt", header =TRUE)
# read data file
aData=read.table("hw05a.txt",header=TRUE)

bData=read.table("hw05b.txt",header=TRUE)
for(i in c(2,3,6)){aData[,i]=as.factor(aData[,i])}
for(i in c(2,3,6)){bData[,i]=as.factor(bData[,i])}
for(i in c(2,3,6)){cData[,i]=as.factor(cData[,i])}

ind = sample(nrow(aData)) %% 10
ind = sample(nrow(bData)) %% 10
ind = sample(nrow(cData)) %% 10
modA=naiveBayes(virus~.,aData[ind!=0,2:6])
modB = naiveBayes(virus~.,bData[ind!=0,2:6])
modC=naiveBayes(virus~.,cData[ind!=0,2:6])
modA
modB
modC
