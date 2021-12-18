set.seed(123456)
rm(list = ls())
#Load in the libraries
library(Matching)
library(tidyverse)

#set working directory to source file location
data <- read_csv("data.csv")
max(data$rf_0622)
min(data$rf_0622)
median(data$rf_0622)
hist(data$rf_0622)
#The out put results were maximum rainfall: 22.72529, minimum rainfall: 0, median rainfall: 0.861068

#In order to apply the binary propensity score matching between control and treatment group,
#I let areas with rain fall less than 0.861068 in the control group 0 and 
#those with rain fall larger than or equal to 0.861068 in the treatment group 1.
#(This seemed justifiable because the rainfall less than the median 0.861068  is very close to having no rain at all,
#marking the areas with rain fall less than the median comparable to actual control group with no rain.)
data$treatment <- as.integer(data$rf_0622>=0.861068)
typeof(data$treatment)
#genetic matching
#Use the same covariates used in the replication of table 4
covariates = cbind(data$Turnout15, data$UKIP14_pct, data$medianAge, data$femPerc, data$percDE, data$percDegree, data$logPop, data$England)
genmatchOutput <- GenMatch(X=covariates, Tr=data$treatment, estimand="ATE")

#genetic matching on remain votes
matchOutput <- Match (Y=data$Remain, X=covariates, Tr=data$treatment, Weight.matrix=genmatchOutput, estimand = "ATE")
matchBalanceOutput <- MatchBalance(data$treatment ~ 
                         data$Turnout15 + data$UKIP14_pct + data$medianAge + data$femPerc + data$percDE + data$percDegree + data$logPop + data$England
                         , data=data, match.out = matchOutput, nboots=1000)

#treatment effect on remain votes
remainATE <- mean(data[matchOutput$index.treated,]$Remain) - mean(data[matchOutput$index.control,]$Remain)

#genetic matching on leave votes
matchOutput <- Match (Y=data$Leave, X=covariates, Tr=data$treatment, Weight.matrix=genmatchOutput, estimand = "ATE")
matchBalanceOutput <- MatchBalance(data$treatment ~ 
                                     data$Turnout15 + data$UKIP14_pct + data$medianAge + data$femPerc + data$percDE + data$percDegree + data$logPop + data$England
                                   , data=data, match.out = matchOutput, nboots=1000)

#treatment effect on leave votes
leaveATE <- mean(data[matchOutput$index.treated,]$Leave) - mean(data[matchOutput$index.control,]$Leave)

#remainATE
remainATE
#leaveATE
leaveATE

