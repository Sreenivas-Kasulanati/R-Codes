setwd("C:\\Users\\Sreenivas\\Desktop\\Graded assignment 3 dataset")  #setting path(Work directory)
gfu<-read.csv("goodforu1-1.csv")                                    #reading file
library(dplyr)                                                      #calling libraries
class(gfu)
summary(gfu)
library(gains)
gfu%>%mutate(Target=ifelse(X24>4,1,0))->gfu1                #converting Target vaiable to binomial
boxplot(X24~Target, data = gfu1)
set.seed(250)
index<-sample(nrow(gfu1),0.70*nrow(gfu1),replace=F)
summary("index")
train<-gfu1[index,]
test<-gfu1[-index,]
mod<-glm(formula = Target~.,data=train[,-66],family="binomial")
summary(mod)
step(mod, direction = "both")
mod1<-glm(formula = Target~X3+X10+X17+X31,data=gfu1,family="binomial")
summary(mod1)
gfu1$X3_d<-ifelse(gfu1$X3 == '1',1,0)                                     #data cleaning(Preparation)
gfu1$X10_d<-ifelse(gfu1$X10 == '1',1,0)                                   #data cleaning(Preparation)
gfu1$X17_d<-ifelse(gfu1$X17 == '1',1,0)                                   #data cleaning(Preparation)
gfu1$X31_d<-ifelse(gfu1$X31 > 4 ,1,0)                                     #data cleaning(Preparation)
mod2<-glm(formula = Target~X3_d+X10_d+X17_d+X31_d,data=gfu1,family="binomial")
summary(mod2)
mod2
