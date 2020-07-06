setwd("C:/Users/Mauri/Documents/CIS 635/Homework/hw3")
dataset = read.table("hw03data.txt", header = TRUE)
View(dataset)
install.packages("car")
install.packages("ggplot2")
install.packages("devtools")
library(devtools)
library(ggplot2)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
for(i in c(2,3,6)){dataset[,i]=as.factor(dataset[,i])}
mean(dataset$pulse)

v = sample(nrow(dataset)) %%3
group1 = v==2

aggregate.data.frame(dataset$pulse, list(dataset$ageGroup), mean)
aggregate(dataset$pulse, list(dataset$ageGroup), mean)


df <- data.frame(id = c("101","102","103","104","105","106", "107","108","109","110"),
                 gender = c("M","M","F","M","F","F","M","F","M","F"),
                 AgeGroup =c(1,3,2,2,1,3,1,2,3,2),
                 pulse = c(67,68,73,77,62,81,70,81,105,63),
                 temp =c(96.1,97.8,98.3,98.9,98.1,99.1,96.9,97.5,98.2,98.6),
                 virus = c("Y","N","N","N","Y","N","N","Y","N","Y"))

aggregate(df$pulse, list(df$AgeGroup), mean)
mean(df$pulse)

pulse <- df$pulse
outlier_values <-boxplot.stats(dataset$pulse)$out

sdpulse <- sd(pulse)
sd3pulse <- sdpulse*3
sd3pulse

nrow(dataset)
summary(dataset)
histogram(dataset$temp)
histogram(dataset$pulse, main = " heart rate")
boxplot.default(dataset$pulse,main = " heart rate")
boxplot(dataset$pulse, subset(dataset$virus))
plot(dataset$temp, dataset$pulse)
boxplot(dataset$pulse,dataset$virus)
virus =dataset[,6]=="Y"
boxplot(dataset[virus,4], main= "heartrate with virus")
boxplot(dataset[virus,4],dataset[!virus,4], main =" comparison of heart rate", names=c("positive", "negative"))

boxplot(dataset[virus,5],dataset[!virus,5], main =" comparison of temperature", names=c("positive", "negative"))
scatter.smooth(dataset$pulse, dataset$temp)
temp <- dataset$temp
pulse <-dataset$pulse
plot(temp,pulse )
virus <- dataset$virus
qplot(pulse,temp, colour = virus, data = dataset)
gender <- dataset$gender
table(gender, virus)
diseased = rep(c("no virus", "virus"))
ggplot(dataset, aes(x=temp, fill= virus))+geom_boxplot()
ageGroup <- dataset$ageGroup
table(ageGroup, virus )
