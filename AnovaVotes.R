library(readxl)

#setwd('V:/Stat382')

data <- read_excel('Cleaned_Data.xlsx')
georgia <- read_excel('Georgia.xlsx')
georgia <- georgia[-160,]
colnames(georgia)
str(data)
str(georgia)


## anova 
EDV <- rowSums(georgia[,c(2,7,12)]) # Election day voting
sum(ED)

VBM <- rowSums(georgia[,c(3,8,13)]) # voting by mail
sum(VBM)

EV <- rowSums(georgia[,c(4,9,14)]) # Early voting 
sum(EV)

data$style_votes <- 0
for(i in 1:159){
  data[i,"style_votes"] <- max(EDV[i],EV[i],VBM[i])
}

data$style <- ''
for(i in 1:159){
  if((EDV[i] > VBM[i]) & (EDV[i] > EV)){
    data[i,"style"] <- 'EDV'
  }
  else if((EV[i] > EDV[i]) & (EV[i] > VBM)){
    data[i,"style"] <- 'EV'
  }
  else{
    data[i,"style"] <- 'VBM'
  }
}
options(scipen=999)
par(cex.axis=0.8, mar=c(8, 4, 5, 2))

boxplot(data$style_votes~data$style, ylim=c(0,60000), xlab='Style', ylab = 'Votes',
        col = c('green','orange','purple'))

voting <- aov(style_votes~style, data = data)
summary(voting)

TukeyHSD(voting)
plot(TukeyHSD(voting))

###Code you can use
#Analyze vote method preference of Biden vs. Trump voters
georgia <- as.data.frame(read_excel('Georgia.xlsx'))[-160,]
Biden_breakdown = c(sum(georgia$`B_Election Day Votes`),sum(georgia$`B_Advanced Voting Votes`),sum(georgia$`B_Absentee by Mail Votes`),sum(georgia$`B_Provisional Votes`))
Trump_breakdown = c(sum(georgia$`T_Election Day Votes`),sum(georgia$`T_Advanced Voting Votes`),sum(georgia$`T_Absentee by Mail Votes`),sum(georgia$`T_Provisional Votes`))
breakdown = cbind(Biden_breakdown,Trump_breakdown)
rownames(breakdown) = c("Election Day", "Early Vote", "Mail", "Provisional")
colnames(breakdown) = c("Biden", "Trump")
#Analyize this and post the results, add in a total column and row when you show the table
chisq.test(breakdown)

#Analyze vote method preference of urban, suburban, and rural voters
georgia$urbanicity = input$urbanicity
georgia$total_ElectionDay = rowSums(georgia[,c(2,7,12)])
georgia$total_Mail = rowSums(georgia[,c(3,8,13)])
georgia$total_Early = rowSums(georgia[,c(4,9,14)])
georgia$total_provisional = rowSums(georgia[,c(5,10,15)])
election = tapply(georgia$total_ElectionDay, georgia$urbanicity, sum)
mail= tapply(georgia$total_Mail, georgia$urbanicity, sum)
early = tapply(georgia$total_Early, georgia$urbanicity, sum)
provisional = tapply(georgia$total_provisional, georgia$urbanicity, sum)
RSU = rbind(election, mail,early, provisional)
rownames(RSU) = c("Election Day", "Mail", "Early vote", "Provisional")

#Analyze this and post the results. Explain which two sets are different. Add in a total column and row when you show the table
chisq.test(RSU)