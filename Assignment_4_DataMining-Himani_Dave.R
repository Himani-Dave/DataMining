#****************************************************************
#Name:Himani Dave
#Student Number:A00178443
#
#BTA1012 Assignment 7
#****************************************************************

############
#Question 1
############

# a) 

ubank <- read.csv("UniversalBankA4.csv", header = TRUE)

sum(is.na(ubank))

#The dataset does not contain any missing values. 

# b) 

dummy_data <- model.matrix(~0 + Family + Education, data = ubank)
dummy_data <- as.data.frame(dummy_data)
ubank.new <- ubank[, -c(6, 8)]
ubank.new <- data.frame(ubank.new, dummy_data)

# c) 

set.seed(111)
training.rows<- sample(1:nrow(ubank.new), nrow(ubank.new)*0.6)
training.data <- ubank.new[training.rows, ]
validation.rows <- setdiff(1:nrow(ubank.new), training.rows)
validation.data <- ubank.new[validation.rows, ]

# d) 

install.packages("BBmisc")
library(BBmisc)
norm.ubank <- normalize(ubank, method = "standardize", margin = 1L)
norm.training <- normalize(training.data, method = "standardize", margin = 1L)
norm.validation <- normalize(validation.data, method = "standardize", margin = 1L)

# e) 

install.packages("e1071")
library(caret)
library(e1071)
training.data$Personal.Loan <- factor(training.data$Personal.Loan)
validation.data$Personal.Loan <- factor(validation.data$Personal.Loan)
accuracy.df.bank <- data.frame(k = seq(1, 50, 1), accuracy = rep(0, 50))
for(i in 1:50) {
  knn.pred <- knn(training.data[, -c(1,5,8)], validation.data[, -c(1,5,8)],  cl = training.data[, 8],
                   k = i)
  accuracy.df.bank[i, 2] <- confusionMatrix(knn.pred, validation.data[, 8])$overall[1] 
}
max(accuracy.df.bank$accuracy)
highest <- subset(accuracy.df.bank$k, accuracy.df.bank$accuracy == max(accuracy.df.bank$accuracy))
highest

# k = 8 gives the highest accuracy of 0.908

# f)

new <- data.frame( Experience = 31, Income = 150, Age = 59, CCAvg = 2.9, Mortgage = 0,
                   Securities.Account = 1, CD.Account = 1,
                   Family = 1,  Education = c(0,1,0), Online = 1, Credit.Card = 0)
knn.pred.new <- knn(norm.ubank[, -c(1,5,8)], new, 
                    cl = norm.ubank[, 8], k = 8)
row.names(training.data)[attr(nn, "nn.index")]