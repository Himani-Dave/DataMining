#****************************************************************
#Name:Himani Dave
#Student Number:A00178443
#
#ANA1001 Lab 1
#****************************************************************

############
#Question 1
############

# 1) Unsupervised learning

# 2) Supervised learning

# 3) Supervised learning

# 4) Unsupervised learning

############
#Question 2 
############

age <- c(25, 56, 65, 32, 41, 49)
income <- c(49000, 156000, 99000, 192000, 39000, 57000)

df <- data.frame(age, income)

norm_age <- (df$age - mean(df$age)) / sd(df$age)
norm_income <- (df$income - mean(df$income)) / sd(df$income)
norm_data <- data.frame(norm_age, norm_income)

scale(norm_data)

############
#Question 3 
############

cars <- read.csv("ToyotaCorolla.csv", header = TRUE)

#dummy data creation

dummy_data <- model.matrix(~0 + Fuel_Type + Color, data = cars)
dummy_data <- as.data.frame(dummy_data)

#removing columns
new_cars <- cars[, -c(8, 11)]
cars <- data.frame(new_cars, dummy_data)

#training and validation data 
training_rows<- sample(1:nrow(cars), nrow(cars)*0.7)
training_data <- cars[training_rows, ]
validation_rows <- setdiff(1:nrow(cars), training_rows)
validation_data <- cars[validation_rows, ]
