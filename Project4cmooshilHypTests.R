#Final Project Update for Friday, 11/20

#Our data set is Georgia voter statistics for the 2020 election.

setwd("/Users/christinamooshil/Downloads/GAData")
data <- read.csv("Cleaned_Data.csv", sep = ",")

str(data)

#Test the hypothesis that the mean of the youth voters in each county in Georgia is equal to 20
#with 5% significance level.

#H0: mu = 20
#H1: mu =/= 20

t.test(data$youth, alternative = "two.sided", mu = hypvalue)

#Conclusion: The p-value is 0.8652, which is greater than our significance level 0.05.
#We do not reject the null hypothesis and we have significant evidence that the mean of the youth voters in each county 
#in Georgia is equal to 20.

#Test the hypothesis that the mean of Turnout (the total votes of the 2020 election)
#is greater than the mean of Turnout2016 (the total votes of the 2016 election) 
#with a 5% significance level.


turnout2016 <- data$Turnout2016
turnout2020 <- data$Turnout


t.test(turnout2020,turnout2016, var.equal=FALSE, alternative = c("greater"), mu = 0, paired = TRUE)

#Conclusion: The p-value is 2.2e-16 which is less than our significance level 0.05, so we
#reject the null hypothesis. There is significant evidence that the mean of Turnout is greater than 
#the mean of Turnout2016.