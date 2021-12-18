set.seed(123456)
rm(list = ls())
#Load in the libraries
library(CBPS)
library(stargazer)
library(texreg)
library(systemfit)
library(readr)

#set working directory to source file location
data <- read_csv("data.csv")
#balancing
balance <- npCBPS(rf_0622~Turnout15 + 
                    UKIP14_pct +
                    medianAge +
                    femPerc +
                    percDE +  
                    percDegree + 
                    logPop +
                    postal_pct +
                    England , 
                  method='exact',
                  data=data)

#### TABLE 1####
mean(abs(balance(balance)$unweighted))
mean(abs(balance(balance)$balanced))

mean<-colMeans(data[,c(33,8,38,18,16,19,15,35,39,31)])

mean<- c(NA,mean(data$Turnout),
         mean(data$leave_pct/(data$leave_pct+data$remain_pct)*100),
         mean(data$remain_pct/(data$leave_pct+data$remain_pct)*100),
         NA,NA, mean[1], NA,NA,mean[2:10])

sd<-sapply(data[,c(33,8,38,18,16,19,15,35,39,31)], sd)
sd<- c(NA,sd(data$Turnout),
       sd(data$leave_pct/(data$leave_pct+data$remain_pct)*100),
       sd(data$remain_pct/(data$leave_pct+data$remain_pct)*100),
       NA,NA, sd[1], NA,NA,sd[2:10])


raw<-balance(balance)$unweighted

raw<- c(NA,
        cor(data$rf_0622,data$Turnout),
        cor(data$rf_0622,data$leave_pct/(data$leave_pct+data$remain_pct)),
        cor(data$rf_0622,data$remain_pct/(data$leave_pct+data$remain_pct)),
        NA, NA, 1, NA, NA, raw)

balanced<-balance(balance)$balanced

balanced<- c(NA,NA,NA,NA, NA,NA,1, NA,NA,balanced)

covlabs<- c('Outcome','Turnout (%)','Leave (%)',
            'Remain (%)','','Treatment',
            'Rain 6am-10pm (mm)','  ',
            'Covariates',
            'Turnout 2015 GE (%)'  ,
            'UKIP 2014 EP (%)',
            'Median Age' ,
            'Women (%)',
            'Low Social Grade (%)' ,
            'Higher Education (%)',
            '$ln$(Pop. Density)',
            'Postal Votes (%)', 'England')


df<- as.data.frame(cbind(mean,sd,raw, balanced), stringsAsFactors = F)

rownames(df)<- covlabs

colnames(df)<- c('Mean', 'Standard Deviation', 'Raw Balance',
                 'Weighted Balance')

stargazer(df,summary=F,rownames = T,
          colnames=T,
          type = 'latex',
          notes= '')














#### TABLE 2 ####

m1<- lm(Turnout~rf_0622+
          Turnout15  +
          UKIP14_pct+
          femPerc+
          percDE +  
          logPop +
          percDegree + 
          medianAge +
          country, 
        data=data)

m2<- lm(Turnout~rf_0622+
          Turnout15  +
          UKIP14_pct+
          femPerc+
          percDE +  
          logPop +
          percDegree + 
          medianAge +
          England, 
        data=data, 
        weights=balance$weights)

m3<- lm(Leave_Share~rf_0622+
          Turnout15  +
          UKIP14_pct+
          femPerc+
          percDE +  
          logPop +
          percDegree + 
          medianAge +
          country, 
        data=data)


m4 <- lm(Leave_Share~rf_0622+
           Turnout15  +
           UKIP14_pct+
           femPerc+
           percDE +  
           logPop +
           percDegree + 
           medianAge +
           England, 
         data=data, 
         weights=balance$weights)

mnames<-c('Model 1','Model 2',
          'Model 3','Model 4')

covlabs2<- c('Rain 6am-10pm (mm)',
             'Turnout 2015 GE (%)'  ,
             'UKIP 2014 EP (%)',
             'Women (%)',
             'Low Social Grade (%)' ,
             '$ln$(Pop. Density)',
             'Higher Education (%)',
             'Median Age',
             'England')

texreg(list(m1,m2,m3,m4),
       custom.coef.names = c('Constant',covlabs2[1:8], rep(NA,3),covlabs2[9] ),
       reorder.coef = c(2:9,13,10:12,1),
       custom.model.names = mnames)




#### TABLE 3 (and Appendix TABLE 3) ####



##m5
X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   country, data = data)


Leave <- log(data$leave_pct/data$abstain_pct)
Remain <-log(data$remain_pct/data$abstain_pct)

m5l<- Leave ~ X-1
m5r<- Remain ~ X-1

m5<- systemfit(list(m5l,m5r),method = 'SUR')

#summary(m6)

#m6


w<- sqrt(balance$weights)
X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w)%*%X

Leave <- diag(w)%*%log(data$leave_pct/data$abstain_pct)
Remain <-diag(w)%*%log(data$remain_pct/data$abstain_pct)

m5l<- Leave ~ X-1
m5r<- Remain ~ X-1

m6<- systemfit(list(m5l,m5r),method = 'SUR')




sub('& 796','& 398',  texreg(list(m5,m6),
                             digits=4, single.row = T))





#### TABLE 4 ####

RLcount <- read_csv("remainLeaveCounts.csv")
data<-merge(data, RLcount, by='Area')

data$Remain<- log(data$remain_pct/data$abstain_pct)
data$Leave<- log(data$leave_pct/data$abstain_pct)

w<- sqrt(balance$weights)
X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w)%*%X

Leave <- diag(w)%*%log(data$leave_pct/data$abstain_pct)
Remain <-diag(w)%*%log(data$remain_pct/data$abstain_pct)

m5l<- Leave ~ X-1
m5r<- Remain ~ X-1

m5<- systemfit(list(m5l,m5r),method = 'SUR')

# neat coefficient based predictions


sunnyLpoint <- data$Leave  - m5[1]$eq[[1]]$coefficients[2]*data$rf_0622
sunnyRpoint <- data$Remain   -  m5[1]$eq[[2]]$coefficients[2]*data$rf_0622

lp <- exp(sunnyLpoint)/(1+exp(sunnyLpoint)+exp(sunnyRpoint))
rp <- exp(sunnyRpoint)/(1+exp(sunnyLpoint)+exp(sunnyRpoint))

la <- exp(data$Leave) /(1+exp(data$Leave)+exp(data$Remain))
ra <- exp(data$Remain)/(1+exp(data$Leave)+exp(data$Remain))


data$sunnyRperc<- rp*100 - ra*100
data$sunnyLperc<- lp*100 - la*100

data$sunny_extra_Rpeople<- round((rp*data$Electorate - ra*data$Electorate))
data$sunny_extra_Lpeople<- round((lp*data$Electorate - la*data$Electorate))



table1<- cbind(data[head(order(data$sunny_extra_Rpeople, 
                              decreasing = T),5),
                   c(1,5,length(data[1,])-1)])


colnames(table1)<- c( 'District', 'Region','Remain votes lost to rainfall')
rownames(table1)<-NULL

table2 <- cbind(data[head(order(data$sunny_extra_Lpeople,
                decreasing = T),5),
                c(1,5,length(data[1,]))])
colnames(table2)<- c('District', 'Region', 'Leave votes lost to rainfall' )
rownames(table2)<-NULL

stargazer(table1, table2, summary=F)


#### Appendix TABLE 1  ####

balance00 <- npCBPS(rf_0022~
                      Turnout15  +
                      UKIP14_pct+
                      femPerc+
                      percDE +  
                      logPop +
                      percDegree + 
                      medianAge +
                      England+postal_pct, 
                    method='exact',
                    data=data)


m1<- lm(Turnout~rf_0022+
          Turnout15  +
          UKIP14_pct+
          femPerc+
          percDE +  
          logPop +
          percDegree + 
          medianAge +
          country, 
        data=data)

m2<- lm(Turnout~rf_0022+
          Turnout15  +
          UKIP14_pct+
          femPerc+
          percDE +  
          logPop +
          percDegree + 
          medianAge +
          England, 
        data=data, 
        weights=balance00$weights)

m3<- lm(Leave_Share~rf_0022+
          Turnout15  +
          UKIP14_pct+
          femPerc+
          percDE +  
          logPop +
          percDegree + 
          medianAge +
          country, 
        data=data)


m4 <- lm(Leave_Share~rf_0022+
           Turnout15  +
           UKIP14_pct+
           femPerc+
           percDE +  
           logPop +
           percDegree + 
           medianAge +
           England, 
         data=data, 
         weights=balance00$weights)

mnames<-c('Model 1','Model 2',
          'Model 3','Model 4')

covlabs2<- c('Rain 12 midnight -10 pm (mm)',
             'Turnout 2015 GE (%)'  ,
             'UKIP 2014 EP (%)',
             'Women (%)',
             'Low Social Grade (%)' ,
             '$ln$(Pop. Density)',
             'Higher Education (%)',
             'Median Age',
             'England')
texreg(list(m1,m2,m3,m4),
       custom.coef.names = c('Constant',covlabs2[1:8], rep(NA,3),covlabs2[9] ),
       reorder.coef = c(2:9,13,10:12,1),
       custom.model.names = mnames)




#### Appendix TABLE 2 ####




X<- model.matrix(~rf_0022+
                   Turnout15+
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   country, data = data)


Leave2 <-log(data$leave_pct/data$abstain_pct)
Remain2 <-log(data$remain_pct/data$abstain_pct)

m2eq1<- Leave2 ~ X-1
m2eq2<- Remain2 ~ X-1

m005 <- systemfit(list(m2eq1,m2eq2),method = 'SUR')



m2.s1 <- npCBPS(rf_0022~
                  Turnout15  +
                  UKIP14_pct+
                  femPerc+
                  percDE +  
                  logPop +
                  percDegree + 
                  medianAge +
                  England+postal_pct, 
                method='exact',
                data=data)

w2<- sqrt(m2.s1$weights)
X<- model.matrix(~rf_0022+
                   Turnout15+
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w2)%*%X

Leave2 <-diag(w2)%*%log(data$leave_pct/data$abstain_pct)
Remain2 <-diag(w2)%*%log(data$remain_pct/data$abstain_pct)

m2eq1<- Leave2 ~ X-1
m2eq2<- Remain2 ~ X-1

m006 <- systemfit(list(m2eq1,m2eq2),method = 'SUR')





sub('& 796','& 398',  texreg(list(m005,m006),
                             digits=4, single.row = T))




#### Appendix TABLE 4 ####

w<- sqrt(balance$weights)

X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England + 
                   postal_pct*rf_0622, data = data)

X<-diag(w)%*%X

Leave <- diag(w)%*%log(data$leave_pct/data$abstain_pct)
Remain <-diag(w)%*%log(data$remain_pct/data$abstain_pct)

m6eq1<- Leave ~ X-1
m6eq2<- Remain ~ X-1

mINT <- systemfit(list(m6eq1,m6eq2),method = 'SUR')


m6.s1 <- npCBPS(minutes_0622~
                  Turnout15  +
                  UKIP14_pct+
                  femPerc+
                  percDE +  
                  logPop +
                  percDegree + 
                  medianAge +
                  England+postal_pct, 
                method='exact',
                data=data)

w6<- sqrt(m6.s1$weights)
X<- model.matrix(~minutes_0622+
                   Turnout15  +
                   UKIP14_pct+
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w6)%*%X

Leave6 <- diag(w6)%*%log(data$leave_pct/data$abstain_pct)
Remain6 <-diag(w6)%*%log(data$remain_pct/data$abstain_pct)

m6eq1<- Leave6 ~ X-1
m6eq2<- Remain6 ~ X-1

mMIN <- systemfit(list(m6eq1,m6eq2),method = 'SUR')




library(texreg)

sub('& 796','& 398',  texreg(list(mINT, mMIN),
                             digits=4, single.row = T))


#### Appendix TABLE 5 ####

#class interaction


w<- sqrt(balance$weights)
X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct +
                   femPerc+
                   percDE*rf_0622 +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w)%*%X

Leave <- diag(w)%*%log(data$leave_pct/data$abstain_pct)
Remain <-diag(w)%*%log(data$remain_pct/data$abstain_pct)

m5l<- Leave ~ X-1
m5r<- Remain ~ X-1

m6cla<- systemfit(list(m5l,m5r),method = 'SUR')

#class interaction


w<- sqrt(balance$weights)
X<- model.matrix(~rf_0622+
                   Turnout15  +
                   UKIP14_pct +
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge*rf_0622 + 
                   England, data = data)

X<-diag(w)%*%X

Leave <- diag(w)%*%log(data$leave_pct/data$abstain_pct)
Remain <-diag(w)%*%log(data$remain_pct/data$abstain_pct)

m5l<- Leave ~ X-1
m5r<- Remain ~ X-1

m6age<- systemfit(list(m5l,m5r),method = 'SUR')

# habit interaction


w<- sqrt(balance$weights)
X<- model.matrix(~rf_0622+
                   Turnout15*rf_0622 +
                   UKIP14_pct +
                   femPerc+
                   percDE +  
                   logPop +
                   percDegree + 
                   medianAge + 
                   England, data = data)

X<-diag(w)%*%X

Leave <- diag(w)%*%log(data$leave_pct/data$abstain_pct)
Remain <-diag(w)%*%log(data$remain_pct/data$abstain_pct)

m5l<- Leave ~ X-1
m5r<- Remain ~ X-1

m6hab<- systemfit(list(m5l,m5r),method = 'SUR')




sub('& 796','& 398',  texreg(list(m6cla,m6age, m6hab),
                             digits=4, single.row = T))


