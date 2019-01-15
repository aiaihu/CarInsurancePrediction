#Title: "Insurance - Claim Analysis"
#Course ID: "CSDA1010"
#Professor: "Mr. Zeyad Azem"
#Authors: "Group Project: Aimin Amy Hu, Weitian Victor, Christopher Tan, Christine Coore"

#import packages for using in this project

library(tidyverse)
library(haven)
library(dplyr)
library(lubridate)
library(ggplot2)
library(quantmod)
library(corrplot)
library(chron)
library(plyr)
library(gplots)
library(ROCR)
library(pROC)

#Part 1: Data Understanding
# Read the car insurance claim csv file.
claim_df=read_csv("Class1010/Lab2/car_insurance_claim.csv")

#save the data frame as a back up
dfBkup=claim_df

head(claim_df)

#check all variables names
names(claim_df)

#check data frame details
str(claim_df)

#checking missing value for the dataset
colSums(is.na(claim_df))


#find percentage of missing/NAs in a data.frame
sum(is.na(claim_df))/prod(dim(claim_df))

#find percentage of missing/NAs in columns
colMeans(is.na(claim_df))


###################
# Part 2: Data Preparation

# clean the data
# Clean up the z_ characters in all the columns
claim_df$MSTATUS <- gsub("z_","", claim_df$MSTATUS)
claim_df$EDUCATION <- gsub("z_","", claim_df$EDUCATION)
claim_df$EDUCATION <- gsub("<","", claim_df$EDUCATION)
claim_df$OCCUPATION <- gsub("z_","", claim_df$OCCUPATION)
claim_df$CAR_TYPE <- gsub("z_","", claim_df$CAR_TYPE)
claim_df$URBANICITY <- gsub("z_","", claim_df$URBANICITY)
claim_df$GENDER <- gsub("z_","", claim_df$GENDER)

str(claim_df)



# Convert date field to proper date format

claim_df$BIRTH=as.Date(chron(format(as.Date(claim_df$BIRTH,"%d%b%y"),"%m/%d/%y")))
str(claim_df$BIRTH)

#remove dollar sign $ from data frame

claim_df[]<-lapply(claim_df,gsub,pattern="\\$",replacement="")

#remove "," from data frame
claim_df[]<-lapply(claim_df,gsub,pattern=",",replacement="")

#Conver characters to numeric for INCOME, HOME_VAL,BLUEBOOK,OLDCLAIM,CLM_AMT
claim_df$INCOME<-as.numeric(claim_df$INCOME)
claim_df$HOME_VAL<-as.numeric(claim_df$HOME_VAL)
claim_df$BLUEBOOK<-as.numeric(claim_df$BLUEBOOK)
claim_df$OLDCLAIM<-as.numeric(claim_df$OLDCLAIM)
claim_df$CLM_AMT<-as.numeric(claim_df$CLM_AMT)
str(claim_df)
summary(claim_df)

#plot each variable relate to past accident
# Variable KIDSDRIV with past accident
ggplot(claim_df,aes(x=KIDSDRIV,fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar()+
  labs(y="Policyholder Count",
       title="past accident rates by KIDSDRIV")

#gender and occupation relations with past accident
ggplot(claim_df,aes(x=GENDER,fill=CLAIM_FLAG))+
  theme_bw()+
  facet_wrap(~OCCUPATION)+
  geom_bar()+
  labs(y="Policyholder Count",
       title="Past Accident rate by gender and occupation")

#use barplot to see age and past accident relation
claim_df$AGE<-as.numeric(claim_df$AGE)
ggplot(claim_df,aes(x=AGE, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by Age")

#plot for YOJ 
claim_df$YOJ<-as.numeric(claim_df$YOJ)
ggplot(claim_df,aes(x=YOJ, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by Years of Job")

#plot variable HOMEKIDS with accident
ggplot(claim_df,aes(x=HOMEKIDS, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by HOMEKIDS")

#plot variable MSTATUS with accident
ggplot(claim_df,aes(x=MSTATUS, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by MSTSTUS")

#plot variable EDUCATION with accident
ggplot(claim_df,aes(x=EDUCATION, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by EDUCATION")

#plot variable TRAVTIME with accident
claim_df$TRAVTIME<-as.numeric(claim_df$TRAVTIME)
ggplot(claim_df,aes(x=TRAVTIME, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by TRAVTIME")

#plot variable CAR_USE with accident
ggplot(claim_df,aes(x=CAR_USE, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by CAR_USE")

#plot variable BLUEBOOK with accident
ggplot(claim_df,aes(x=BLUEBOOK, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_histogram(bins=30) +
  labs(y="Policyholder Count",
       x="BLUEBOOK",
       title = "Accident Rates by BLUEBOOK")

#plot variable TIF with accident
claim_df$TIF<-as.numeric(claim_df$TIF)
ggplot(claim_df,aes(x=TIF, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by TIF")

#plot variable CAR_TYPE with accident
ggplot(claim_df,aes(x=CAR_TYPE, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by Car Type")

#plot variable RED_CAR with accident
ggplot(claim_df,aes(x=RED_CAR, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       title = "Accident Rates by Red Car")

#plot variable OLDCLAIM with accident

ggplot(claim_df,aes(x=OLDCLAIM, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_histogram(bins=20) +
  labs(y="Policyholder Count",
       x="OLDCLAIM",
       title = "Accident Rates by Old Claim")

#plot variable CLM_FREQ with accident
ggplot(claim_df,aes(x=CLM_FREQ, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar() +
  labs(y="Policyholder Count",
       x="CLM_FREQ",
       title = "Accident Rates by CLM_FREQ")

#plot variable REVOKED with accident
ggplot(claim_df,aes(x=REVOKED, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar()+
  labs(y="Policyholder Count",
       title = "Accident Rates by REVOKED")


#plot variable MVR_PTS with accident
claim_df$MVR_PTS<-as.numeric(claim_df$MVR_PTS)
ggplot(claim_df,aes(x=MVR_PTS, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar()+
  labs(y="Policyholder Count",
       title = "Accident Rates by MVR_PTS")

#plot variable CAR_AGE with accident
claim_df$CAR_AGE<-as.numeric(claim_df$CAR_AGE)
ggplot(claim_df,aes(x=CAR_AGE, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar()+
  labs(y="Policyholder Count",
       title = "Accident Rates by Car Age")

#plot variable URBANICITY with accident
ggplot(claim_df,aes(x=URBANICITY, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_bar()+
  labs(y="Policyholder Count",
       title = "Accident Rates by Urbanicity")

#plot variable INCOME with accident
ggplot(claim_df,aes(x=INCOME, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_histogram(bins=30) +
  labs(y="Policyholder Count",
       x="INCOME",
       title = "Accident Rates by INCOME")

#plot variable HOME_VAL with accident
ggplot(claim_df,aes(x=HOME_VAL, fill=CLAIM_FLAG))+
  theme_bw()+
  geom_histogram(bins=30) +
  labs(y="Policyholder Count",
       x="HOME_VAL",
       title = "Accident Rates by Home Value")

#############################################

## Part 2: Data Preparation
#Step 1: Converting categorical variables to numeric numbers

#Converting GENDER variable to numeric: male=1, female=0
claim_df$GENDER[claim_df$GENDER=="M"] <- "1"
claim_df$GENDER[claim_df$GENDER=="F"] <- "0"

#need to convert character to numeric
claim_df$GENDER<-as.numeric(claim_df$GENDER)

#Converting PARENT1 variable to numeric: Yes=1, No=0
claim_df$PARENT1[claim_df$PARENT1=="Yes"] <- "1"
claim_df$PARENT1[claim_df$PARENT1=="No"] <- "0"

#need to convert character to numeric
claim_df$PARENT1<-as.numeric(claim_df$PARENT1)


#Converting MSTATUS variable to numeric: Yes=1, No=0
claim_df$MSTATUS[claim_df$MSTATUS=="Yes"] <- "1"
claim_df$MSTATUS[claim_df$MSTATUS=="No"] <- "0"

#need to convert character to numeric
claim_df$MSTATUS<-as.numeric(claim_df$MSTATUS)


#Converting CAR_USE variable to numeric: Commercial =1, Private =0
claim_df$CAR_USE[claim_df$CAR_USE=="Commercial"] <- "1"
claim_df$CAR_USE[claim_df$CAR_USE=="Private"] <- "0"

#need to convert character to numeric
claim_df$CAR_USE<-as.numeric(claim_df$CAR_USE)

#Converting RED_CAR variable to numeric: yes =1, no=0
claim_df$RED_CAR[claim_df$RED_CAR=="yes"] <- "1"
claim_df$RED_CAR[claim_df$RED_CAR=="no"] <- "0"

#need to convert character to numeric
claim_df$RED_CAR<-as.numeric(claim_df$RED_CAR)


#Converting REVOKED variable to numeric: Yes=1, No=0
claim_df$REVOKED[claim_df$REVOKED=="Yes"] <- "1"
claim_df$REVOKED[claim_df$REVOKED=="No"] <- "0"

#need to convert character to numeric
claim_df$REVOKED<-as.numeric(claim_df$REVOKED)

#Converting URBANICITY variable to numeric: Highly Urban/ Urban =1
#Highly Rural/ Rural=0
claim_df$URBANICITY[claim_df$URBANICITY=="Highly Urban/ Urban"] <- "1"
claim_df$URBANICITY[claim_df$URBANICITY=="Highly Rural/ Rural"] <- "0"

#need to convert character to numeric
claim_df$URBANICITY<-as.numeric(claim_df$URBANICITY)

#Converting EDUCATION variale to numeric: High School=1, Bachelors=2
#Masters=3,PhD=4
claim_df$EDUCATION[claim_df$EDUCATION=="High School"] <- "1"
claim_df$EDUCATION[claim_df$EDUCATION=="Bachelors"] <- "2"
claim_df$EDUCATION[claim_df$EDUCATION=="Masters"] <- "3"
claim_df$EDUCATION[claim_df$EDUCATION=="PhD"] <- "4"

#need to convert character to numeric
claim_df$EDUCATION<-as.numeric(claim_df$EDUCATION)


#Converting CAR_TYPE variable to numeric: Minivan or Van = 1, Sport Cars=2
#Suv=3, Panel Truck =4, Pickup=5
claim_df$CAR_TYPE[claim_df$CAR_TYPE=="Minivan"|claim_df$CAR_TYPE=="Van"] <- "1"
claim_df$CAR_TYPE[claim_df$CAR_TYPE=="Sports Car"] <- "2"
claim_df$CAR_TYPE[claim_df$CAR_TYPE=="SUV"] <- "3"
claim_df$CAR_TYPE[claim_df$CAR_TYPE=="Panel Truck"] <- "4"
claim_df$CAR_TYPE[claim_df$CAR_TYPE=="Pickup"] <- "5"

#need to convert character to numeric
claim_df$CAR_TYPE<-as.numeric(claim_df$CAR_TYPE)



#replace NA with Not Specified for OCCUPATION
claim_df$OCCUPATION<-claim_df$OCCUPATION %>% replace_na(("Not Specified"))

#Converting OCCUPATION variable to numeric:Not Specified =0, Student =1,Blue Collar or Home Maker=2
#Manager or Clerical=3, Professional =4, Doctor or Lawyer =5

claim_df$OCCUPATION[claim_df$OCCUPATION=="Not Specified"] <- "0"
claim_df$OCCUPATION[claim_df$OCCUPATION=="Student"] <- "1"
claim_df$OCCUPATION[claim_df$OCCUPATION=="Home Maker" |claim_df$OCCUPATION=="Blue Collar" ] <- "2"
claim_df$OCCUPATION[claim_df$OCCUPATION=="Manager" |claim_df$OCCUPATION=="Clerical" ] <- "3"
claim_df$OCCUPATION[claim_df$OCCUPATION=="Professional"] <- "4"
claim_df$OCCUPATION[claim_df$OCCUPATION=="Doctor" |claim_df$OCCUPATION=="Lawyer" ] <- "5"
#need to convert character to numeric
claim_df$OCCUPATION<-as.numeric(claim_df$OCCUPATION)

#need to convert character to numeric
claim_df$KIDSDRIV<-as.numeric(claim_df$KIDSDRIV)
claim_df$HOMEKIDS<-as.numeric(claim_df$HOMEKIDS)
claim_df$CLM_FREQ<-as.numeric(claim_df$CLM_FREQ)
claim_df$CLAIM_FLAG<-as.numeric(claim_df$CLAIM_FLAG)

##Step 2: Dealing with missing values

#calculate median for age
agemedian<-median(claim_df$AGE,na.rm = TRUE)

#replace NA with median for AGE
claim_df$AGE<-claim_df$AGE %>% replace_na((agemedian))

#Replace negative values with absolute value for CAR_AGE
claim_df$CAR_AGE[claim_df$CAR_AGE==-3]<-3

#Replace zero with 1 for CAR_AGE
claim_df$CAR_AGE[claim_df$CAR_AGE==0]<-1

#calculate median for CAR_AGE
carage_median<-median(claim_df$CAR_AGE,na.rm = TRUE)
mean(claim_df$CAR_AGE,na.rm = TRUE)

#Replace NA with median for CAR_AGE
claim_df$CAR_AGE<-claim_df$CAR_AGE %>% replace_na((carage_median))

#Replace missing value by mean for INCOME variable
incomeMean = claim_df$INCOME %>% mean(na.rm=TRUE)
claim_df$INCOME <- claim_df$INCOME %>% replace_na(incomeMean)

#Replace missing value by mean for HOME_VAL variable
homeValueMean = claim_df$HOME_VAL %>% mean(na.rm=TRUE)
claim_df$HOME_VAL <- claim_df$HOME_VAL %>% replace_na(homeValueMean)

#Replace missing value by mean for YOJ variable
YOJMean = claim_df$YOJ %>% mean(na.rm=TRUE)
claim_df$YOJ <- claim_df$YOJ %>% replace_na(YOJMean)

#Step 3: Correlation matrix
#subset dataset without ID,BIRTH and CLM_AMT
cl_df<-subset(claim_df,select=-c(ID, BIRTH,CLM_AMT))


#correlation matrix
round(cor(cl_df),3)
mdf<-round(cor(cl_df),3)

#plot for correlation matrix
library(corrplot)
corrplot(mdf, method = "circle",insig = "blank")
           
#anoth way to find out strong correlation between variables
#drop duplicates and meaningless information
mdf[lower.tri(mdf,diag = TRUE)]=NA

matrix_df<-as.data.frame(as.table(mdf))

library(reshape)
#drop perfect
matrix_df[matrix_df==1]<-NA
#subset correlation matrix with absolute greater than 0.4
matrix_df10<-subset(matrix_df,abs(Freq)>0.45)
#sorting
matrix_df10[order(-abs(matrix_df10$Freq)),]


#Step 4: Selecting Data

#Select data: get a subset dataset with ID and 14 independent variables and CLAIM_FLAG only. 
#attribute ID is not a variable, it is just a record indicator.
df_final<-subset(claim_df,select=c(ID,EDUCATION,CAR_AGE,GENDER,RED_CAR,INCOME,
               HOME_VAL,OLDCLAIM,CLM_FREQ,PARENT1,MSTATUS,KIDSDRIV,
               HOMEKIDS,OCCUPATION,CAR_USE,CLAIM_FLAG))

df_new_BKUP=df_final

# Part 3: Build Models
#Split training and testing data: train:60%, test:40%

library(caTools)

#To set a seed number
set.seed(1)

#Split dataset
spl<-sample.split(Y=df_final$ID,SplitRatio = 0.6)
spl
train=subset(df_final,spl=="TRUE")
test=subset(df_final,spl=="FALSE")

#Step 1: creating a model: Logistic Regression with all 14 variables
model<-glm(CLAIM_FLAG~.-ID,train,family="binomial")
summary(model)

#Step 2:Remodeling without CAR_AGE

model<-glm(CLAIM_FLAG~.-ID-CAR_AGE,train,family="binomial")
summary(model)

#Step3: Remodeling without RED_CAR

model<-glm(CLAIM_FLAG~.-ID-RED_CAR,train,family="binomial")
summary(model)

# Step 4: Remodeling without OCCUPATION

model<-glm(CLAIM_FLAG~.-ID-OCCUPATION,train,family="binomial")
summary(model)



#Step 5 :use the fitted model to do predictions for the test data
library(ROSE)
res<-predict(model,data=test, type="response")
res


#Step 6:Evaluate the model by creating confusion matrix according to threshold
#First:Use ROC Curve to find threshold 
#import library for ROC for finding thr
library(pROC)
#predicted values for training dataset
pred<-predict(model,data=train, type="response")

#defind the RocPred and RocPref
#tpr=ture positive rate.   fpr=falls positive rate
RocPred<-prediction(pred,train$CLAIM_FLAG)
RocPref<-performance(RocPred,"tpr","fpr")
plot(RocPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1),main="ROC Curve")

#Second: creating confusion matrix according to threshold at 0.5
tab<-table(Actualvalue=train$CLAIM_FLAG,Predictedvalue=res>0.5)
tab

confusion_matrix<-sum(diag(tab))/sum(tab)*100
confusion_matrix

###########################

#Model III: Decision Tree
#import packages
library(MASS)
library(rpart)

#setting the seed number so we get same results each time
#we run decision tree
set.seed(1)

#save df_final as df_dt for Decision Tree
df_dt=df_final

#create the train and test data set: trainDF=60%, testDF=40%
library(caTools)
library(caret)
library(rpart.plot)
#traget variable is CLAIM_FLAG
ind<-sample.split(Y=df_dt$ID,SplitRatio = 0.6)
trainDF=subset(df_dt,ind=="TRUE")
testDF=subset(df_dt,ind=="FALSE")


#fitting the model
dtmodel<-rpart(CLAIM_FLAG~.-ID, data=trainDF,method = "class")
rpart.plot(dtmodel,type=4,extra = 101)
summary(dtmodel)

library(pROC)
#Predictions
PredictDT<-predict(dtmodel,data=testDF,type="class")

tabDT<-table(actual=trainDF$CLAIM_FLAG,predictions=PredictDT)
tabDT


# Confusion matrix
DT_confusionMT<-sum(diag(tabDT))/sum(tabDT)*100
DT_confusionMT




