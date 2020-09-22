pacman::p_load(xlsx, dplyr, tidyverse)

setwd("/Users/suannnnn/Desktop/NUS ISS/Year 2/CA/Grad Cert")

insurance <- read.csv("WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")

head(insurance)

colnames(insurance)
colnames(insurance) <- str_replace_all(colnames(insurance),"[.]","")
colnames(insurance)

str(insurance)

#There is a mix of continuous and categorical variables
#There are no missing values
#Total of 9134 observations with 24 Variables

# Data Exploration of Customer Life Time value

range(insurance$CustomerLifetimeValue)
mean(insurance$CustomerLifetimeValue)
sd(insurance$CustomerLifetimeValue)
summary(insurance$CustomerLifetimeValue)
boxplot(insurance$CustomerLifetimeValue,ylab = 'Customer Life Time Value', main='Boxplot Distribution of Customer Life Time Value')

library(tidyverse) 
library(car) 
library(zoo)
library(lmtest) 
library(dplyr) 
library(stringr)
library(caret)
library(ggplot2) 
library(timeDate)
#Check the variance, skewness and kurtosis of the Cutomer Life Time Value
var(insurance$CustomerLifetimeValue)
skewness(insurance$CustomerLifetimeValue)
#Value of 3.03, which indicates that the data is positively skewed and most values are concentrated on the left hand of the mean
kurtosis(insurance$CustomerLifetimeValue) 
#Value of 13.88113, which indidcates that the data is heavy-tailed compared to normal distribution and many outliers
hist(insurance$CustomerLifetimeValue, breaks = (max(insurance$CustomerLifetimeValue) - min(insurance$CustomerLifetimeValue))/100, freq = FALSE, main = "CLV Histogram", xlab = "CLV", border = "#FF5733")
#from the histogram, it shows that there are many customers with low CLV and a few cusotmers with high clv

#Analysis of Monthly Premium Auto
range(insurance$MonthlyPremiumAuto)
mean(insurance$MonthlyPremiumAuto)
sd(insurance$MonthlyPremiumAuto)
summary(insurance$MonthlyPremiumAuto)
boxplot(insurance$MonthlyPremiumAuto,ylab = 'Monthly Premium Auto', main='Boxplot Distribution of Monthly Premium Auto')

#Check the variance, skewness and kurtosis of Monthly Premium Auto
var(insurance$MonthlyPremiumAuto)
skewness(insurance$MonthlyPremiumAuto)
#Value of 2.12 , which indicates that the data is positively skewed and most values are concentrated on the left hand of the mean
kurtosis(insurance$MonthlyPremiumAuto) 
#Value of 6.19, which indidcates that the data is heavy-tailed compared to normal distribution and many outliers
cor(insurance$MonthlyPremiumAuto,insurance$CustomerLifetimeValue)
#correlation value of 0.39 indicates that monthly premium auto and customer lifetime value are positively correlated
#This can be further reinforced by a scatter plot
#Plot a histogram of Monthly Premium Auto
hist(insurance$MonthlyPremiumAuto, breaks = (max(insurance$MonthlyPremiumAuto) - min(insurance$MonthlyPremiumAuto))/1, freq = FALSE, main = "Monthly Premium Histogram", xlab = "Monthly Premium", border = "#00AFBB")
#Plot scatter plot between Monthly Premium Auto and Customer Lifetime Value
plot(x=insurance$MonthlyPremiumAuto, y=insurance$CustomerLifetimeValue, col="#00AFBB", cex=1, xlab="MonthlyPremiumAuto", ylab="CustomerLifetimeValue",
     main="Scatterplot of MPA vs CLTV")

#Analysis of Total Claim Amount
#Check the variance, skewness and kurtosis of Total Claim Amount
var(insurance$TotalClaimAmount)
skewness(insurance$TotalClaimAmount)
#Value of 1.71, which indicates that the data is positively skewed and most values are concentrated on the left hand of the mean
kurtosis(insurance$TotalClaimAmount) 
#Value of 5.97, which indidcates that the data is heavy-tailed compared to normal distribution and many outliers
cor(insurance$TotalClaimAmount,insurance$CustomerLifetimeValue)
#correlation value of 0.223 indicates that Total Claim Amount and customer lifetime value are positively correlated
#This can be further reinforced by a scatter plot between Total Claim Amount and Customer Lifetime Value
#Plot a histogram of Total Claim Amount
hist(insurance$TotalClaimAmount, breaks = (max(insurance$TotalClaimAmount) - min(insurance$TotalClaimAmount))/1, freq = FALSE, main = "Total Claim Amount Histogram", xlab = "Total Claim Amount", border = "#00AFBB")
#Plot scatter plot between Total Claim Amount and Customer Lifetime Value
plot(x=insurance$TotalClaimAmount, y=insurance$CustomerLifetimeValue, col="#00AFBB", cex=1, xlab="MonthlyPremiumAuto", ylab="CustomerLifetimeValue",
     main="Scatterplot of Total Claim Amount vs CLTV")

#Since our dependent variable is Customer Lifetime Value, it is important to find out the variables that contribute to a high CLTV customer
#We explore how coverage affects Customer Lifetime Value
ggplot(insurance, aes(x=Coverage, y= CustomerLifetimeValue, fill = Coverage)) + 
  geom_boxplot() + 
  labs(x="Coverage",y = "Customer Life Time Value", fill="Coverage") + 
  ggtitle("Visualization of CLV wrt Coverage")

aggData <- aggregate(x = insurance$CustomerLifetimeValue, by=list(Coverage = insurance$Coverage), FUN = sum)
aggData
ggplot(data = aggData, aes(x = Coverage, y = prop.table(stat(aggData$x)), fill = Coverage, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Coverage', y = 'CLTV in Percentage', fill = 'Coverage') + 
  ggtitle("CLTV Distribution by Coverage")

#We explore if employment status affects Customer Lifetime Value
ggplot(insurance, aes(x=EmploymentStatus, y= CustomerLifetimeValue, fill = EmploymentStatus)) + 
  geom_boxplot() + 
  labs(x="EmploymentStatus",y = "Customer Life Time Value", fill="EmploymentStatus") + 
  ggtitle("Visualization of CLTV wrt EmploymentStatus")

aggData <- aggregate(x = insurance$CustomerLifetimeValue, by=list(EmploymentStatus = insurance$EmploymentStatus), FUN = sum)
aggData
ggplot(data = aggData, aes(x = EmploymentStatus, y = prop.table(stat(aggData$x)), fill = EmploymentStatus, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'EmploymentStatus', y = 'CLTV in Percentage', fill = 'EmploymentStatus') + 
  ggtitle("CLTV Distribution by EmploymentStatus")
#Customers that are employed has higher proportion of 64.1% of CLTV compared to the rest

#We explore if Vehicle Class affects Customer Lifetime Value
ggplot(insurance, aes(x=VehicleClass, y= CustomerLifetimeValue, fill = VehicleClass)) + 
  geom_boxplot() + 
  labs(x="VehicleClass",y = "Customer Life Time Value", fill="VehicleClass") + 
  ggtitle("Visualization of CLTV wrt Vehicle Class")

aggData <- aggregate(x = insurance$CustomerLifetimeValue, by=list(VehicleClass = insurance$VehicleClass), FUN = sum)
aggData
ggplot(data = aggData, aes(x = VehicleClass, y = prop.table(stat(aggData$x)), fill = VehicleClass, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'VehicleClass', y = 'CLTV in Percentage', fill = 'VehicleClass') + 
  ggtitle("CLTV Distribution by Vehicle Class")
#Customers having four door car contributes to higher CLTV
#Explore if gender affects CLTV
ggplot(insurance, aes(x=Gender, y= CustomerLifetimeValue, fill = Gender)) + 
  geom_boxplot() + 
  labs(x="Gender",y = "Customer Life Time Value", fill="Gender") + 
  ggtitle("Visualization of CLTV wrt Gender")

aggData <- aggregate(x = insurance$CustomerLifetimeValue, by=list(Gender = insurance$Gender), FUN = sum)
aggData
ggplot(data = aggData, aes(x = Gender, y = prop.table(stat(aggData$x)), fill = Gender, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Gender', y = 'CLTV in Percentage', fill = 'Gender') + 
  ggtitle("CLTV Distribution by Gender")

#Explore if sales channel affect CLTV
ggplot(insurance, aes(x=SalesChannel, y= CustomerLifetimeValue, fill = SalesChannel)) + 
  geom_boxplot() + 
  labs(x="SalesChannel",y = "Customer Life Time Value", fill="SalesChannel") + 
  ggtitle("Visualization of CLTV wrt SalesChannel")

aggData <- aggregate(x = insurance$CustomerLifetimeValue, by=list(SalesChannel= insurance$SalesChannel), FUN = sum)
aggData
ggplot(data = aggData, aes(x = SalesChannel, y = prop.table(stat(aggData$x)), fill = SalesChannel, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'SalesChannel', y = 'CLTV in Percentage', fill = 'SalesChannel') + 
  ggtitle("CLTV Distribution by SalesChannel")


