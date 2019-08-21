#****************************************************************
#Name:Himani Dave
#Student Number:A00178443
#
#BTA1012 Assignment 5
#****************************************************************

############
#Question 1
############

# a)

delays <- read.csv("FlightDelays.csv", header = TRUE)
delays$DAY_WEEK <- as.factor(delays$DAY_WEEK)

# b)

set.seed(1234)
train.index <- sample(row.names(delays), 0.6*dim(delays)[1])  
valid.index <- setdiff(row.names(delays), train.index)  
train.df <- delays[train.index, ]
valid.df <- delays[valid.index, ]

# c)

library(rpart)
library(rpart.plot)

class.tree <- rpart(Flight.Status ~ CARRIER + DEST + DISTANCE + FL_NUM + ORIGIN + Weather + DAY_WEEK, 
                    data = train.df, 
                    control = rpart.control(maxdepth = 6, cp = 0.001), 
                    method = "class")
prp(class.tree, type = 1, extra = 1, split.font = 1)  

# d) 

#Flight info : WEATHER=0, CARRIER=MQ, DAY_WEEK=4, FL_NUM=3206, ORIGIN=BWI
#Using the decision tree, the above flight would be classified as: Delayed

# e)

class.tree <- rpart(Flight.Status ~ CARRIER + DEST + DISTANCE + FL_NUM + ORIGIN + Weather + DAY_WEEK, 
                    data = train.df, 
                    control = rpart.control(cp = 0.001), 
                    method = "class")

# f) 

library(caret)
default.pred.validation <- predict(class.tree,valid.df,type = "class")

# g) 

status.df <- data.frame(valid.df$Flight.Status, default.pred.validation)
status.sub <- subset(status.df, valid.df.Flight.Status == default.pred.validation)
#The subset has 700 observations.  
#Percentage of accurately predicted flights:
percent <- ((700)/881)*100
percent
#Thus, 79.45% of flights were accurately predicted. 