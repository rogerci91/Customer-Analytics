#install.packages('pacman')
pacman::p_load(lubridate, tidyverse, reshape,dplyr,ggplot2,moments,corrplot,caTools,car,caret,ROCR,earth,ROSE)

#set directory
setwd("~/Desktop/EBA5003/CA")

insurance=read.csv('WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv', na.strings=c(""))
head(insurance)
dim(insurance)
str(insurance)
sapply(insurance, class)
sum(is.na(insurance))
summary(insurance)

#CLTV data exploration
skewness(insurance$Customer.Lifetime.Value)
kurtosis(insurance$Customer.Lifetime.Value)
hist(insurance$Customer.Lifetime.Value,xlab='CLTV',main='Histogram of CLTV')

#correlation between CLTV and other variables
insurance=insurance[,-c(1)]
insurance2=Filter(is.numeric,insurance)
colnames(insurance2)=c(1,2,3,4,5,6,7,8)

#1=CLTV, 2=income, 3=monthly premium, 4=months since last claim, 5=month since policy inception,
#6=number of open complaints, 7=number of policies, 8=total claim amount
insurance_corr=cor(insurance2,use = "complete.obs")
corrplot(insurance_corr,method="color")
corrplot(insurance_corr,method="number")

#which sales channel most effective
insurance%>%
  group_by(Sales.Channel)%>%
  summarise(avgCLTV=mean(Customer.Lifetime.Value))%>%
  ggplot(aes(x=Sales.Channel,y=avgCLTV))+
  geom_bar(stat='identity')

#CLTV highly skewed, transform into log form

insurance$Customer.Lifetime.Value2=log10(insurance$Customer.Lifetime.Value)

skewness(insurance$Customer.Lifetime.Value2)
kurtosis(insurance$Customer.Lifetime.Value2)
hist(insurance$Customer.Lifetime.Value2,xlab='log(CLTV)',main='Histogram of log(CLTV)')

# one hot encoding
dmy <- dummyVars(" ~ . -Customer.Lifetime.Value", data = insurance, fullRank = T)
data2 <- data.frame(predict(dmy, newdata = insurance))

insurance <- cbind(data2, CLTV = insurance$Customer.Lifetime.Value)

str(insurance)

#remove unnecessary variables
insurance3=insurance[,-c(3,6)]
head(insurance3)

#split into training and test
set.seed(100)
splitData = sample.split(insurance3$Customer.Lifetime.Value2, SplitRatio = 0.7)
train_set = insurance3[splitData,]
nrow(train_set)/nrow(insurance3)
test_set = insurance3[!splitData,] 
nrow(test_set)/nrow(insurance3)

#build linear regression model
lm=lm(Customer.Lifetime.Value2~.,data=train_set)
summary(lm)

lm2=lm(Customer.Lifetime.Value2~.-Customer.Lifetime.Value-State-EmploymentStatus-Education-Gender-Income-Months.Since.Last.Claim-Months.Since.Policy.Inception-Policy.Type-Policy-Total.Claim.Amount-Vehicle.Size-Sales.Channel,data=train_set)
summary(lm2)

predictTest =  (predict(lm2, newdata=test_set))
predictTest2=10^predictTest
predictTest2

RMSE =  sqrt(mean((predictTest2 - test_set$Customer.Lifetime.Value)^2))
RMSE

#try without log CLTV
lm3=lm(Customer.Lifetime.Value~.-Customer.Lifetime.Value2-State-EmploymentStatus-Education-Gender-Income-Months.Since.Last.Claim-Months.Since.Policy.Inception-Policy.Type-Policy-Total.Claim.Amount-Vehicle.Size-Sales.Channel,data=train_set)
summary(lm3)

predictTest = predict(lm3, newdata=test_set)
predictTest

RMSE =  sqrt(mean((predictTest - test_set$Customer.Lifetime.Value)^2))
RMSE
