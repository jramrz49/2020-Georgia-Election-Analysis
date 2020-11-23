#Project 4
#Christina Mooshil
#Summary Statistics

setwd("/Users/christinamooshil/Downloads/GAData")
data <- read.csv("Cleaned_Data.csv", sep = ",")

str(data)

#Summary of all variables:

summary(data)

#*****************************************************************

#Catagorical Variables 

#Frequency Tables
table(data$past_result)
table(data$current_result)


#Proportion 

table(data$past_result)/length(data$past_result)
table(data$current_result)/length(data$current_result)


#Contingency Table for past_result/current_result and urbanticity

table(data$past_result, data$urbanicity)
table(data$current_result, data$urbanicity)

#*****************************************************************

#Quantitative Variables (Education, Income)

#Means

mean(data$Education)
mean(data$Median.household.income)


#Variance

var(data$Education)
var(data$Median.household.income)

 
#Standard Deviation

sd(data$Education)
sd(data$Median.household.income)


#Minimums and Maximums

min(data$Education)
min(data$Median.household.income)

max(data$Education)
max(data$Median.household.income)


#Range

range(data$Education)
range(data$Median.household.income)

#Quantiles

quantile(data$Education, probs=c(0.20, 0.5, 0.90, 1))
quantile(data$Median.household.income, probs=c(0.20, 0.5, 0.90, 1))


#Summary

summary(data$Education)
summary(data$Median.household.income)
