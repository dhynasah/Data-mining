setwd("C:/Users/Mauri/Documents/CIS 635")
a =5
v = c(3,6,8,12)
print(v)


orders<- c("L","M","L","S","M","L","L")
orderFact = factor(orders)
orderFact

price = c(10,8,11,9,10,6,11)
means = tapply(price,orderFact, mean)
means

x = matrix(v,nrow = 2)

v=c(1,2,3,4,5,6)
iq = rnorm(35,110,6)
iq

x[1:10,2:4]

iq = rnorm(100,mean=100,sd=20)



iqadd = iq+10
mean(iqadd)
sd(iqadd)

mean(iq)
sd(iq)


onT = rnorm(10000, mean =100, sd = 20)
mean(onT)
sd(onT)

