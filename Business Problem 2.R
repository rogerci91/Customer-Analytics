pacman::p_load(tidyverse, caret, corrplot, caTools,knitr,car,ROCR,IRdisplay, e1071, earth)

setwd("C:/Users/Zhi yang/Documents/MTEC/data")

insurance<-read.csv("carinsurance.csv")

#datacleaning

apply(insurance, 2, function(col)sum(is.na(col)))

# there are no missing values 

str(insurance)

summary(insurance)

dim(insurance)
#there are 9134 customers and 24 variables


data1 <- subset(insurance, select = -c(Customer, Policy.Type, Effective.To.Date))

# one hot encoding
dmy <- dummyVars(" ~ . -Customer.Lifetime.Value", data = data1, fullRank = T)
data2 <- data.frame(predict(dmy, newdata = data1))

insurance <- cbind(data2, CLTV = data1$Customer.Lifetime.Value)


str(insurance)
#data exloration



insurance%>%
  group_by(Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Response,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

# Most customers do not renew rather than renew 


# Check how does each variable affect the response of the customers

insurance%>%
  group_by(State,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=State,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#largest renewals in California and oregon
#largest proportion of renewals in Arizona,California,Oregon


#Looking at coverage

insurance%>%
  group_by(Gender,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Gender,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#14% of both males and females have renewed their subscription

#Employment status

insurance%>%
  group_by(EmploymentStatus,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=EmploymentStatus,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#Largest number of responses from Employed people. Highest proportion is onretired people 27% of people renewed their insurance plan 

#Coverage vs response


insurance%>%
  group_by(Coverage ,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Coverage ,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#Most customers use basic coverage rather than extended or premium 

# coverage with different policy

insurance%>%
  group_by(Coverage ,Policy)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Coverage ,y=count,fill=Policy))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

# it seems that there are different coverage for the same policys


#Plotting coverage and the policy type
insurance%>%
  group_by(Coverage ,Policy.Type)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Coverage ,y=count,fill=Policy.Type))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

# There is basic,extended and Premium coverage in all three types


insurance%>%
  group_by(Coverage ,Policy.Type)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Coverage ,y=count,fill=Policy.Type))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#Sales channel and renewal



insurance%>%
  group_by(Sales.Channel ,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Sales.Channel ,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

# most people bought their insurance through their agent, then by visiting a branch then call center and the web.

#renew offer type

insurance%>%
  group_by(Renew.Offer.Type ,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Renew.Offer.Type ,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#offer 2 has the highest number of responses

#Vehicle class
insurance%>%
group_by(Vehicle.Class  ,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Vehicle.Class  ,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

# Largest number of customers are insuring 4 door cards, However largest number of respondants comes from customers using SUVs
#Vehicle size

insurance%>%
  group_by(Vehicle.Size  ,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Vehicle.Size  ,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#Most vehicles are midsized, However largest proportion of responses are large vehicles

#Location code

insurance%>%
  group_by(Location.Code ,Response)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Location.Code  ,y=count,fill=Response))+geom_bar(stat='identity')+geom_text(aes(label=round(count)))

#most customers come from suburban customerslargest proportion also come from them


#histogram of income 

insurance%>%
  group_by(Income,Response)%>%
  ggplot(aes(x=Income,color=Response))+geom_histogram()

##Monthly premium 

insurance%>%
  group_by(Monthly.Premium.Auto,Response)%>%
  ggplot(aes(x=Monthly.Premium.Auto,color=Response))+geom_histogram()

# most premiums are between 0-150 permonth

#Months since last claim


insurance%>%
  group_by(Months.Since.Last.Claim,Response)%>%
  ggplot(aes(x=Months.Since.Last.Claim,color=Response))+geom_histogram()

# Even response from the customers Month since last claim does not seemto affect

#Months since policy inception


insurance%>%
  group_by(Months.Since.Policy.Inception,Response)%>%
  ggplot(aes(x=Months.Since.Policy.Inception,color=Response))+geom_histogram()

# Even response from the customers Month since last claim does not seemto affect

insurance%>%
  group_by(Months.Since.Last.Claim,Response)%>%
  ggplot(aes(x=Months.Since.Last.Claim,color=Response))+geom_histogram()


insurance%>%
  group_by(Total.Claim.Amount,Response)%>%
  ggplot(aes(x=Total.Claim.Amount,color=Response))+geom_histogram()

#Removal of variables which do not need modeling remove CLTV and remove customer ID

insurance_modeling<-insurance[,c(-1,-3)]
insurance_modeling






#Most important seem to be the renew offer. 

install.packages("cluster")
install.packages("dplyr")
install.packages("readr")
install.packages("Rtsne")
library(cluster)
library(dplyr)
library(readr)
library(Rtsne)


#removing effective date from dataset

#removal of effecive date
insurance_cluster<-insurance
#removal of response,CLTV,Offer,
summary(insurance_cluster)

#using the gowers distance to identify the optimum number of clusters

gower_dist <- daisy(insurance_cluster, metric = "gower")
gower_mat <- as.matrix(gower_dist)


# most similar customers
insurance_cluster[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]


#most disimilar customers
insurance_cluster[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


# using silhouette width to identify the optimum number of clusters

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)


# 3 clusters have the largest silhouette width hence k means should use 3 clusters

#summary of the 3 clusters

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_fit

pam_results1 <- insurance_cluster %>%
  mutate(cluster = pam_fit$clustering)

pam_results <- insurance_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

write.csv(pam_results1,"insurance_cluster.csv")
write.csv(pam_results$the_summary,"2_clusters.csv")


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))




set.seed(123)
str(insurance)
splitData = sample.split(insurance, SplitRatio = 0.5)
train_set = insurance[splitData,]
nrow(train_set)/nrow(insurance)
test_set = insurance[!splitData,]
nrow(test_set)/nrow(insurance)

#

gower_dist <- daisy(train_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)


# 3 clusters have the largest silhouette width hence k means should use 3 clusters

#summary of the 3 clusters

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_fit

pam_results1 <- insurance_cluster %>%
  mutate(cluster = pam_fit$clustering)

pam_results <- insurance_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary




tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#Test Set

gower_dist <- daisy(test_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)


# 3 clusters have the largest silhouette width hence k means should use 3 clusters

#summary of the 3 clusters

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_fit

pam_results1 <- insurance_cluster %>%
  mutate(cluster = pam_fit$clustering)

pam_results <- insurance_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary




tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#Size of clusters

pam_results1%>%
  group_by(cluster)%>%
  summarise(count=n())%>%
  ggplot(aes(x=cluster,y=count))+geom_bar(stat="identity")+geom_text(aes(label=round(count)))

#3858 in cluster 1,2421 in cluster 2 and 2855 in cluster 3

# Number in state of california
pam_results1%>%
  group_by(cluster,State.California)%>%
  summarise(sum=sum(State.California))%>%
  ggplot(aes(x=cluster,y=sum))+geom_bar(stat="identity")+geom_text(aes(label=round(sum)))
# 1950 in cluster 1 922 in cluster 2 278 in cluster 3 


#Number in state of Nevada
pam_results1%>%
  group_by(cluster,State.Nevada)%>%
  summarise(sum=sum(State.Nevada))%>%
  ggplot(aes(x=cluster,y=sum))+geom_bar(stat="identity")+geom_text(aes(label=round(sum)))
# 383 in cluster 1 237 in cluster 2 262 in cluster 3


#Number in state of Oregon
pam_results1%>%
  group_by(cluster,State.Oregon)%>%
  summarise(sum=sum(State.Oregon))%>%
  ggplot(aes(x=cluster,y=sum))+geom_bar(stat="identity")+geom_text(aes(label=round(sum)))

# 441 in cluster 1 606 in cluster 2 1554 in cluster 3


#Number in state of washington 
pam_results1%>%
  group_by(cluster,State.Washington)%>%
  summarise(sum=sum(State.Washington))%>%
  ggplot(aes(x=cluster,y=sum))+geom_bar(stat="identity")+geom_text(aes(label=round(sum)))

# 324 in cluster 1, 226 in cluster 2 and 251 in cluster 3

#State of arizona
#760 in cluster 1, 430 in cluster 2, 510 in cluster 3 

#Number of yes responses
pam_results1%>%
  group_by(cluster,Response.Yes)%>%
  summarise(sum=sum(Response.Yes))%>%
  ggplot(aes(x=cluster,y=sum))+geom_bar(stat="identity")+geom_text(aes(label=round(sum)))
# 630 in cluster 1, 305 in cluster 2, 373 in cluster 3

#Number of no responses
pam_results1%>%
  group_by(cluster,Response.Yes)%>%
  summarise(count1=length(which(Response.Yes==0)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 3228 in cluster 1, 2116 in cluster 2, 2482 in cluster 3

#Coverage.Extended
pam_results1%>%
  group_by(cluster,Coverage.Extended)%>%
  summarise(count1=length(which(Coverage.Extended==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#1192 in cluster 1, 739 in cluster 2,811 in cluster 3

#Coverage.Premium
pam_results1%>%
  group_by(cluster,Coverage.Premium)%>%
  summarise(count1=length(which(Coverage.Premium==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#360 in cluster 1, 210 in cluster 2, 254 in cluster 3

#COverage.Basic
#2306 in cluster 1, 1472 in cluster 2,1790 in cluster 3


#Education.College
pam_results1%>%
  group_by(cluster,Education.College)%>%
  summarise(count1=length(which(Education.College==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#1181 in cluster 1, 626 in cluster 2, 874 in cluster 3

#Education.Doctor
pam_results1%>%
  group_by(cluster,Education.Doctor)%>%
  summarise(count1=length(which(Education.Doctor==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#158 in cluster 1, 32 in cluster 2, 152 in cluster 3

#Education.High.School.or.Below
pam_results1%>%
  group_by(cluster,Education.High.School.or.Below)%>%
  summarise(count1=length(which(Education.High.School.or.Below==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#912 in cluster 1, 1063 in cluster 2, 647 in cluster 3

#Education.Master
pam_results1%>%
  group_by(cluster,Education.Master)%>%
  summarise(count1=length(which(Education.Master==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 362 in cluster 1, 77 in cluster 2, 302 in cluster 3


#Education.Bachelor
#1245 in cluster 1, 318 in cluster 2, 880 in cluster 3


#EmploymentStatus.Employed
pam_results1%>%
  group_by(cluster,EmploymentStatus.Employed)%>%
  summarise(count1=length(which(EmploymentStatus.Employed==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#2971 in cluster 1, 292 in cluster 2, 2435 in cluster 3

#EmploymentStatus.Medical.Leave
pam_results1%>%
  group_by(cluster,EmploymentStatus.Medical.Leave)%>%
  summarise(count1=length(which(EmploymentStatus.Medical.Leave==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 214 in cluster 1, 110 in cluster 2, 108 in cluster 3

#EmploymentStatus.Retired
pam_results1%>%
  group_by(cluster,EmploymentStatus.Retired)%>%
  summarise(count1=length(which(EmploymentStatus.Retired==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 137 in cluster 1, 101 in cluster 2, 44 in cluster 3

#EmploymentStatus.Unemployed
pam_results1%>%
  group_by(cluster,EmploymentStatus.Unemployed)%>%
  summarise(count1=length(which(EmploymentStatus.Unemployed==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#323 in cluster 1, 1841 in cluster 2, 153 in cluster 3.

# disabled
#213 in cluster 1, 77 in cluster 2,115in cluster 3

#Gender.M
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Gender.M==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 1710 in cluster 1, 1522 in cluster 2, 1244 in cluster 3

#Gender.F
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Gender.M==0)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#2148 in cluster 1, 899 in cluster 2, 1611 in cluster 3


#income

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Income))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# cluster 1 median income is 41238 cluster 2 is zero while cluster 3 is 52679

#Location.Code.Suburban
pam_results1%>%
  group_by(cluster,Location.Code.Suburban)%>%
  summarise(count1=length(which(Location.Code.Suburban==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#2607 in cluster 1, 2275 in cluster 2, 897 in cluster 3


#Location.Code.Urban
pam_results1%>%
  group_by(cluster,Location.Code.Urban,Location.Code.Suburban)%>%
  summarise(count1=length(which(Location.Code.Urban==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#591 in cluster 1, 80 in cluster 2, 911 in cluster 3

#Location Rural.
pam_results1%>%
  group_by(cluster,Location.Code.Urban,)%>%
  summarise(count1=length(which(Location.Code.Urban==0 & Location.Code.Suburban==0)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#660 in cluster 1, 66 in cluster 2,1047 in cluster 3

#Marital.Status.Married
pam_results1%>%
  group_by(cluster,Marital.Status.Married)%>%
  summarise(count1=length(which(Marital.Status.Married==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#2871 in cluster 1, 372 in cluster 2 , 2055 in cluster 3

#Marital.Status.Single
pam_results1%>%
  group_by(cluster,Marital.Status.Single)%>%
  summarise(count1=length(which(Marital.Status.Single==1)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 346 in cluster 1, 1756 in cluster 2, 365 in cluster 3

#Divorced
pam_results1%>%
  group_by(cluster,Marital.Status.Single,Marital.Status.Married)%>%
  summarise(count1=length(which(Marital.Status.Single==0 & Marital.Status.Married==0)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#641 in cluster 1, 293 in cluster 2, 435 in cluster 3.


#Monthly.Premium.Auto

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Monthly.Premium.Auto))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 83 is cluster 1, 84 is cluster 2, 81 is cluster 3

#Months.Since.Last.Claim

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Months.Since.Last.Claim))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 14 is cluster 1, 14 is cluster 2, 14 is cluster 3

#Months.Since.Policy.Inception

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Months.Since.Policy.Inception))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 48 is cluster 1, 48 is cluster 2, 48 is cluster 3

#Number.of.Open.Complaints

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Number.of.Open.Complaints))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 0 is cluster 1, 0 is cluster 2, 0 is cluster 3

#Number.of.Policies

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Number.of.Policies))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 2 is cluster 1, 2 is cluster 2, 2 is cluster 3

#Policy.Corporate.L2
pam_results1%>%
  group_by(cluster,Policy.Corporate.L2)%>%
  summarise(count1=length(which(Policy.Corporate.L2==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 215 is cluster 1, 181 is cluster 2, 199 is cluster 3

#Policy.Corporate.L3
pam_results1%>%
  group_by(cluster,Policy.Corporate.L3)%>%
  summarise(count1=length(which(Policy.Corporate.L3==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#359 in cluster1, 318 in cluster2, 337 in cluster 3

#Policy.Personal.L1
pam_results1%>%
  group_by(cluster,Policy.Personal.L1)%>%
  summarise(count1=length(which(Policy.Personal.L1==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#409 in cluster 1, 404 in cluster 2, 427 in cluster 3


#Policy.Personal.L2
pam_results1%>%
  group_by(cluster,Policy.Personal.L2)%>%
  summarise(count1=length(which(Policy.Personal.L2==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#282 in cluster 1, 571 in cluster 2,1269 in cluster3

#Policy.Personal.L3

pam_results1%>%
  group_by(cluster,Policy.Personal.L3)%>%
  summarise(count1=length(which(Policy.Personal.L3==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#2340 in cluster1, 708 in cluster 2, 378 in cluster3

#Policy.Special.L1

pam_results1%>%
  group_by(cluster,Policy.Special.L1)%>%
  summarise(count1=length(which(Policy.Special.L1==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#26 in cluster1, 16 in cluster 2, 24 in cluster 3

#Policy.Special.L2

pam_results1%>%
  group_by(cluster,Policy.Special.L2)%>%
  summarise(count1=length(which(Policy.Special.L2==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#51 in cluster 1, 50 in cluster 2, 63 in cluster 3.


#Policy.Special.L3

pam_results1%>%
  group_by(cluster,Policy.Special.L3)%>%
  summarise(count1=length(which(Policy.Special.L3==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#45 in cluster 1, 55 in cluster 2, 48 in cluster 3


# Policy Corporate L1
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Policy.Special.L3==0&Policy.Special.L2==0 &Policy.Special.L1==0&Policy.Personal.L3==0&Policy.Personal.L2==0&Policy.Personal.L1==0&Policy.Corporate.L3==0&Policy.Corporate.L2==0 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 131 in cluster 1, 118 in cluster 2, 110 in cluster 3


#Renew offer 1
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Renew.Offer.Type.Offer2==0&Renew.Offer.Type.Offer3==0&Renew.Offer.Type.Offer4==0 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#1451 in cluster 1, 1285 in cluster 2, 1016 in cluster 3


#Renew offer 2
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Renew.Offer.Type.Offer2==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#1355 in cluster 1, 556 in cluster 2, 1015 cluster 3

#Renew offer 3
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Renew.Offer.Type.Offer3==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#592 in cluster 1, 376 in cluster 2, 464 in cluster 3

#Renew offer 4
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Renew.Offer.Type.Offer4==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 460 in cluster 1, 204 in cluster 2,360 in cluster 3


#Sales.Channel.Branch

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Sales.Channel.Branch==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 1073 in cluster 1, 669 in cluster 2, 825 in cluster 3


#Sales.Channel.Call.Center
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Sales.Channel.Call.Center==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 778 cluster 1, 445 in cluster 2, 542 in cluster 3


#Sales.Channel.Web
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Sales.Channel.Web==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#555 in cluster 1, 352 in cluster 2, 418 in cluster 3


#Sales.Channel.Agent
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Sales.Channel.Web==0&Sales.Channel.Call.Center==0&Sales.Channel.Branch==0 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#1452 in cluster 1, 955 in cluster 2, 1070 in cluster 3


#Total.Claim.Amount

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Total.Claim.Amount))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 379 in cluster1, 526 in cluster 2, 286 in cluster 3


#Vehicle.Class.Luxury.Car
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Class.Luxury.Car==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 76in cluster 1, 51 in cluster 2, 36 in cluster 3


#Vehicle.Class.Luxury.SUV
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Class.Luxury.SUV==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 77 in cluster 1, 51 in cluster 2, 56 in cluster 3


#Vehicle.Class.Sports.Car
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Class.Sports.Car==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
# 199 in cluster 1, 137 in cluster 2, 148 in cluster 3

#Vehicle.Class.SUV
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Class.SUV==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#746 cluster 1, 503 in cluster 2, 547 in cluster 3


#Vehicle.Class.Two.Door.Car
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Class.Two.Door.Car==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

# 798 in cluster 1, 488 in cluster 2, 600 in cluster 3


# Vehicle.Class.4 door car
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Class.Two.Door.Car==0&Vehicle.Class.SUV==0&Vehicle.Class.Sports.Car==0&Vehicle.Class.Luxury.SUV==0&Vehicle.Class.Luxury.Car==0)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#1962 in cluster 1, 1191 in cluster 2, 1468 in cluster 3


#Vehicle.Size.Medsize
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Size.Medsize==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#2621 in cluster 1, 1686 in cluster 2, 2117 in cluster 3

#Vehicle.Size.Small
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Size.Small==1 )))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))
#823 in cluster 1, 493 in cluster 2, 448 in cluster 3


#Vehicle.Size.Large 
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=length(which(Vehicle.Size.Small==0&Vehicle.Size.Medsize==0)))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#414 in cluster 1, 242 in cluster 2, 290 in cluster 3


#Net.value

pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(Net.value))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))

#5362 in cluster 1, 5217 in cluster 2, 5629 in cluster 3


#CLTV
pam_results1%>%
  group_by(cluster)%>%
  summarise(count1=median(CLTV))%>%
  ggplot(aes(x=cluster,y=count1))+geom_bar(stat="identity")+geom_text(aes(label=round(count1)))



#K means
install.packages("klaR")
library(klaR)

cluster.results <-kmodes(insurance_cluster[,2:20], 4, iter.max = 10, weighted = FALSE )

cluster.results

cluster.output <- cbind(insurance,cluster.results$cluster)
write.csv(cluster.output, file = "kmodes clusters.csv", row.names = TRUE)

# clusters plot
install.packages("cluster")
library(cluster)

clusplot(cluster.output,cluster.results$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


#Decision tree classifyer

pam_results1_cleaned<-pam_results1[,c(-5,-6,-7,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-41,-49,-50)]

str(pam_results1)
str(pam_results1_cleaned)


# converting continuous to bins

pam_results1_cleaned$Income.bin <- cut(pam_results1_cleaned$Income, breaks = 4, labels = c("Level1", "Level2", "Level3","Level4"))
str(pam_results1_cleaned)
pam_results1_cleaned<-pam_results1_cleaned[,-14]

#hot encoding income.bin


data1 <- subset(pam_results1_cleaned, select = c(Income.bin))
data1
# one hot encoding
dmy <- dummyVars(" ~ .", data = data1, fullRank = T)
data2 <- data.frame(predict(dmy, newdata = data1))
data2
pam_results1_cleaned <- cbind(pam_results1_cleaned,data2)

str(pam_results1_cleaned)

pam_results1_final<-pam_results1_cleaned[,-29]
str(pam_results1_final)


#splitting into test and train dataet.
set.seed(123)
splitData = sample.split(pam_results1_final$cluster, SplitRatio = 0.7)
train_set = pam_results1_final[splitData,]
nrow(train_set)/nrow(pam_results1_final)
test_set = pam_results1_final[!splitData,]
nrow(test_set)/nrow(pam_results1_final)

#decision tree building
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(cluster~., data = train_set, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, test_set, type = 'class')

table_mat <- table(test_set$cluster, predict_unseen)
table_mat

#Accuracy

total_accuracy<-((863+615+620)/(863+80+207+56+615+30+238+31+620))*100
total_accuracy