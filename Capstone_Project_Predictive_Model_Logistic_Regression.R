# 1.) Logistic Regression/Classifcation Model

setwd("C:/Users/india/Desktop/R_practice/Data")

ccd_train = read.csv("credit_card_default_train.csv", stringsAsFactors = F)
ccd_test = read.csv("credit_card_default_test.csv", stringsAsFactors = F)

library(dplyr)
library(ggplot2)

# Data Preparation

ccd_test$default.payment.next.month = NA 

ccd_train$data = "train"
ccd_test$data = "test"

ccd_all = rbind(ccd_train, ccd_test)

head(ccd_all)
tail(ccd_all)

names(ccd_all)

#Verifying any missing values in the data

sapply(ccd_all, function(x) sum(is.na(x)))

glimpse(ccd_all)

table(ccd_all$MARRIAGE)

#Verify the length of the Unique values in the table

sapply(ccd_all,function(x) length(unique(x)))

#Verify the columns which are of categorical type

names(ccd_all)[sapply(ccd_all, function(x) is.character(x))]

# convert the variables to Categorical which can be converted

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_cols = c("SEX", "EDUCATION","MARRIAGE", "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5",
             "PAY_6")

for (cat in cat_cols) {
  ccd_all = CreateDummies(ccd_all, cat, 50)
}

glimpse(ccd_all)

sum(sapply(ccd_all,function(x) is.character(x)))

table(ccd_all$default.payment.next.month)

#Separate dataset to begin Modelling process

ccd_train = ccd_all %>% filter(data == 'train') %>% select(-data)
ccd_test = ccd_all %>% filter(data == 'test') %>% select(-data, -default.payment.next.month)

set.seed(2)
s = sample(1 : nrow(ccd_train), 0.8 * nrow(ccd_train))
ccd_train1 = ccd_train[s,]
ccd_train2 = ccd_train[-s,]

# VIF values of the predictor (VIF Cutoff is 10)

library(car)

for_vif = lm(default.payment.next.month ~. - ID, data = ccd_train1)

sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-1
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-2
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-3
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-4
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-5
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-6
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-7
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-8
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-9
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0-BILL_AMT5, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-10
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0-BILL_AMT5-BILL_AMT2, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-11
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0-BILL_AMT5-BILL_AMT2
             -BILL_AMT3, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Final Iteration
for_vif = lm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0-BILL_AMT5-BILL_AMT2
             -BILL_AMT3-BILL_AMT4, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#All VIF Values are less than 10

#Building Classification Model to estimate the tentative Performance

log_fit = glm(default.payment.next.month ~. - ID-PAY_5_0-PAY_4_0-MARRIAGE_2
              -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0-BILL_AMT5-BILL_AMT2
              -BILL_AMT3-BILL_AMT4, data = ccd_train1, family = "binomial")


log_fit = step(log_fit)

summary(log_fit)

formula(log_fit)

log_fit = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + PAY_0_1 + 
                PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + 
                PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + 
                PAY_6_2 + PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

# Run the above model and drop variables based on the p-values (cutoff is 0.05)

#Iteration - 1

log_fit = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + PAY_0_1 + 
                PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + 
                PAY_6_2 + PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

#Iteration - 2
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + PAY_0_1 + 
                PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + 
                PAY_6_2 + PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

#Iteration - 3
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + PAY_0_3 + PAY_0_2 + PAY_0_1 + 
                PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + 
                PAY_6_2 + PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

#Final Iteration
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + PAY_0_3 + PAY_0_2 + PAY_0_1 + 
                PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + 
                PAY_6_2 + PAY_6__2, data = ccd_train1, family = "binomial")

summary(log_fit)

# Predict the model on the train2 data
library(pROC)

val.score = predict(log_fit, newdata = ccd_train2, type = 'response')

auc_score = auc(roc(ccd_train2$default.payment.next.month, val.score))

auc_score

# So the tentative performance of Logistic Regression is going to be around 0.7658

#Visualizing through plot

mydata = data.frame( default.payment.next.month = ccd_train2$default.payment.next.month,
                     val.score = val.score)
ggplot(mydata, aes(x= val.score, y=default.payment.next.month,
                   color = factor(default.payment.next.month)))+geom_point()+geom_jitter()

# Building the Model on entire Training Data
# VIF values of the predictor (VIF Cutoff is 10)
library(car)

for_vif = lm(default.payment.next.month ~. -ID, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-1
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-2
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-3
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-4
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-5
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-6
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-7
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-8
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-9
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0
             -BILL_AMT2, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-10
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0
             -BILL_AMT2-BILL_AMT5, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration-11
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0
             -BILL_AMT2-BILL_AMT5-BILL_AMT3, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Final - Iteration
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
             -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0
             -BILL_AMT2-BILL_AMT5-BILL_AMT3-BILL_AMT4, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

# Building Model on Trained data

log.fit.final = glm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
                    -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0
                    -BILL_AMT2-BILL_AMT5-BILL_AMT3-BILL_AMT4, 
                    data = ccd_train, family = "binomial")

log.fit.final = step(log.fit.final)

summary(log.fit.final)

formula(log.fit.final)

# Run the above model and drop variables based on the p-values (cutoff is 0.05)

#Iteration - 1
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1, 
                      data = ccd_train, family = "binomial")
summary(log.fit.final)

#Iteration - 2
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1, 
                    data = ccd_train, family = "binomial")
summary(log.fit.final)

#Iteration - 3
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_6_2 + PAY_6__2 + PAY_6__1, 
                    data = ccd_train, family = "binomial")
summary(log.fit.final)


#Iteration - 4
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_6_2 + PAY_6__2, 
                    data = ccd_train, family = "binomial")
summary(log.fit.final)

#Iteration - 5
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_6_2 + PAY_6__2, 
                    data = ccd_train, family = "binomial")
summary(log.fit.final)

#Iteration - 6
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_6_2 + PAY_6__2, 
                    data = ccd_train, family = "binomial")
summary(log.fit.final)

#Final - Iteration 
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + MARRIAGE_1 + PAY_0_3 + PAY_0_2 + 
                      PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + 
                      PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_2 + 
                      PAY_6_2 + PAY_6__2, 
                    data = ccd_train, family = "binomial")
summary(log.fit.final)

#To submit simple probability score we can make prediction on test data and submit 

test.prob.score = predict(log.fit.final, newdata = ccd_test, type = 'response')
write.csv(test.prob.score, "ccd_lr_submission.csv", row.names = F)

# To Predict Hard classes instead of probabilities

train.score = predict(log.fit.final, newdata = ccd_train, type = "response")
#head(train.score)
real = ccd_train$default.payment.next.month
cutoffs = seq(0.001, 0.999, 0.001)

cutoff_data = data.frame(cutoff = 99, Sn = 99, Sp= 99, KS= 99, F5= 99, F.1= 99, M = 99)

for (cutoff in cutoffs){
  predicted = as.numeric(train.score > cutoff)
  TP = sum(real == 1 & predicted == 1)
  TN = sum(real == 0 & predicted == 0)
  FP = sum(real == 0 & predicted == 1)
  FN = sum(real == 1 & predicted == 0)
  
  P = TP + FN
  N = TN + FP
  
  Sn = TP/P
  Sp = TN/N
  precision = TP/(TP + FP)
  recall = Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
  
}

cutoff_data = cutoff_data[-1,]

ggplot(cutoff_data, aes(x=cutoff, y=F.1))+geom_line()

# Visualize all of them at once
library(tidyr)
cutoff_long = cutoff_data %>% 
  gather(Measure, Value, Sn:M)

ggplot(cutoff_long, aes(x=cutoff, y=Value, color = Measure))+geom_line()

# Determined cutoff for F1-

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$F.1)]
my_cutoff

#my_cutoff for F1 score is 0.765

#Convert predicted probability scores for the test to hardclasses -
test.predicted = as.numeric(test.prob.score > my_cutoff)
write.csv(test.predicted, "f1_ccd_lr_submission.csv",row.names=F)


