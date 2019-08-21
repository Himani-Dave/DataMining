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

cosmetics.raw <- read.csv("Cosmetics.csv", header = TRUE)
cosmetics.mat <- as.matrix(cosmetics.raw[, -1])
cosmetics.trans <- as(cosmetics.mat, "transactions")
inspect(cosmetics.trans[1:10])

# b)

rules <- apriori(cosmetics.trans, parameter = list(supp = 0.3, conf = 0.55, target = "rules"))

# 4 such rules are generated. 

# c) Information from these rules can be compressed by eliminating redundant and useless rules.

# d) Business Owners can utilize these association rules to market and suggest assoicated product.
# In the given example, one of the rule suggests association between Mascara and Eye Shadow.
# Thus, if a customer is buying one of them, the business owner can suggest them to buy the other as well
# as there are high chances of the customer buying the other product.

# e)

rules_re <- apriori(cosmetics.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))
inspect(sort(rules_re, by = "lift"))

# More rules (14) are generated compared to part b. 

# f) Taking the association rule between mascara and eye shadow, we are 89.91% sure that if 
# a customer buys a mascara, then she will buy an eye shadow. The lift ratio is 2.36 which shows it is 
# is greater than one, thus suggesting that they are quite likely to be bought together. 
#The support says that the itemset is 32.1% popular. 