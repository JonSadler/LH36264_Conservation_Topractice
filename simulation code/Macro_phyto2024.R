library(tidyverse)
library(car)
library(performance)
library(ggfortify)
library(MuMIn)
# Simulated phytoplankton pond dataset
# 2024 data
reps<-factor(c(rep(seq(1,20),4)))
w<-round(rnorm(20,mean=20,sd=7),0)
w1<-round(rnorm(20,mean=40,sd=10),0)
c<-round(rnorm(20,mean=15,sd=6),0)
c1<-round(rnorm(20,mean=20,sd=8),0)

SR<-c(w,w1,c,c1)
SR[SR<0]<-0
temp<-c(rep("warm",40),rep("ctrl",40))
m_phyte<- rep(c("absent","present"),2, each=20)
AllData<-data.frame(reps,SR,temp,m_phyte)

ANOVAresults<-aov(SR~temp*m_phyte, AllData)

summary(ANOVAresults)

model.tables(ANOVAresults,"means")

interaction.plot(temp,m_phyte,SR) 

autoplot(ANOVAresults, which=1:6) # looks good

# write datafile
write.csv(AllData,"Warming2024.csv",row.names = FALSE) 
