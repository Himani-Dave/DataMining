#****************************************************************
#Name:Himani Dave
#Student Number:A00178443
#
#BTA1012 Assignment 6
#****************************************************************

############
#Question 1
############


mowers <- read.csv("RidingMowers.csv", header = TRUE)
logit.reg <- glm(as.factor(Ownership) ~ Income + Lot_Size, data = mowers, family = "binomial") 
options(scipen = 999)
summary(logit.reg)

Full <- data.frame(mowers, round(data.frame(logit.reg$fitted.values),2))
Full$classification <- rep(0,24)
for (i in 1:24) {if (Full$logit.reg.fitted.values[i]>0.5) {Full$classification[i]<-Full$classification[i]+1}}
Full$actual <- rep(0,24)
for (i in 1:24) {if (Full$Ownership[i]=="Owner") {Full$actual[i]<-Full$actual[i]+1}}



New <- read.csv(file.choose())
NewPred <- predict(logit.reg, New[,-3], type = "response")
data.frame(NewPred)

