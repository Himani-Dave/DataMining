#****************************************************************
#Name:Himani Dave
#Student Number:A00178443
#
#ANA1001 Lab 2
#****************************************************************

airfares <- read.csv("Airfares.csv", header = TRUE)

############
#Question 1
############

# a) 

cor_matrix <- cor(airfares[,c(5,9:13, 16:18)]) 
plot(airfares$DISTANCE, airfares$FARE)
abline(lm(airfares$FARE~airfares$DISTANCE))

#The single best predictor for fares would be distance. 

# b) 

airfares$S_CODE<-NULL
airfares$S_CITY<- NULL
airfares$E_CODE<-NULL
airfares$E_CITY<-NULL


# c)

dummy_var <- model.matrix(~0 +VACATION+SW+SLOT+GATE, data = airfares)
dummy_var <- as.data.frame(dummy_var)
airfares[,c(3,4,10,11)]<-NULL
airfares<- data.frame(airfares, dummy_var)
airfares$VACATIONNo <- NULL


# d)

train_ind <- sample(c(1:638),400)
train_datafr <- airfares[train_ind, ]
valid_datafr <- airfares[-train_ind,]


# e) 

airfares.lm1 <- lm(FARE ~., data= train_datafr)
options(scipen=999)
summary(airfares.lm1)


# f) 

install.packages("forecast")
library(forecast)
airfare_lm1_pred <- predict(airfares.lm1, valid_datafr)
options(scipen = 999, digit = 0)
residuals <- valid_datafr$FARE-airfare_lm1_pred
predictionSummary <- data.frame("Predicted"= airfare_lm1_pred,
                                "Actual"= valid_datafr$FARE, "Residual" = residuals)
options(scipen = 999, digits = 3)
head(predictionSummary, n = 15)

# g) 

hist(residuals)

# It is distributed normally. 


# h)

#accuracy (airfare_lm1_pred, valid_datafr$FARE)

# i) 

route <- data.frame(1.202, 3, 4442.141, 28760, 27664, 4557004, 3195503, 1976, 12782, NA,0,
                       0,1,1)
colnames(route) <- colnames(airfares)
route.pred <- predict(airfares.lm1, route)
route.pred