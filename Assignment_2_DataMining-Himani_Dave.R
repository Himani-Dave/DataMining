#****************************************************************
#Name:Himani Dave
#Student Number:A00178443
#
#ANA1001 Lab 1
#****************************************************************


cereal <- read.table(file.choose(), header=TRUE, sep="")

############
#Question 1
############

# a)


#The quantitative variables are : calories, protein, fat, sodium, carbo, sugars, potass, vitamins
#shelf, weight, cups, rating, fibre

# b) 

MeanMedian<- data.frame(mean=sapply(cereal[,4:16],mean), median=sapply(cereal[,4:16], median))
MeanMedian$Differences <- abs(MeanMedian$mean - MeanMedian$median)

#Based on this, we can say that sodium might have a skew. 

# c)

greatest <- table(cereal$mfr)
greatest

# K(Kelloggs) has the greatest number of crereals.

# d)

#Cold
csub <- subset(cereal, cereal$type=="C")
mean(csub$calories)
hist(csub$calories)

#Hot
hsub <- subset(cereal, cereal$type=="H")
mean(hsub$calories)
hist(hsub$calories)

aggregate(cereal$calories, by = list(Type=cereal$type), FUN = mean)

#On average, cold cereals has slightly higher calories. 

# e)

aggregate(cereal[,16], by = list(Manufacturer =cereal$mfr), FUN = mean)

#N has the highest average rating. 

# f) 

aggregate(cereal[,c(5,9,6)], by = list(Manufacturer =cereal$mfr), FUN = mean)

# g)

aggregate(cereal$protein, by = list(Manufacturer =cereal$mfr), FUN = function(x) length(x[x>=4]))

############
#Question 2
############

x<- round(cor(cereal[,4:16]),2)
for (i in 1:13) {for(j in 1:13)
{if (i<=j)
{x[i,j]<-0}}}
install.packages("gplots")
library(gplots)
heatmap.2(x, Rowv = FALSE, Colv = FALSE, dendrogram = "none", cellnote = x, 
          notecol = "black", key = FALSE, trace = "none", margins=c(10,10) )