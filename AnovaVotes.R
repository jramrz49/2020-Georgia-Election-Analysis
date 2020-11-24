library(readxl)

setwd('V:/Stat382')

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
