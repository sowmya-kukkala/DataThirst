# Need to build a predictive model to asses whether a credit card account holder is going to default 
#on their payments next month , on the basis of some personal information and their past payment patterns . 


getwd()
setwd("C:/Users/india/Desktop/R_practice/Data")

## 1.Load the data
ccd_train = read.csv("credit_card_default_train.csv", stringsAsFactors = F)
ccd_test = read.csv("credit_card_default_test.csv", stringsAsFactors = F)

library(dplyr)
library(ggplot2)

# Since there is an absence of Target variable in test data we will add the column assigning the values with NA

ccd_test$default.payment.next.month = NA

# Now we can see that the ccd_train and ccd_test dataframes has equal number of variables

# Add a new column in both the dataframes to split the them according the values in the column

ccd_train$data = "train"
ccd_test$data = "test"

# For data pre-processing we can do horizontal stacking on the given dataframes 

ccd_all = bind_rows(ccd_train, ccd_test)

#Peep into the head and tail of the dataframe

head(ccd_all)

tail(ccd_all)



# Have a look at the first few values with the datatypes of the columns

str(ccd_all)

# Statistical summary on the data frame

summary(ccd_all)

#alternative to view the statistical summary
library(psych)

#Which provides the additional summary including the Central Tendency, variability and skewness of data
describe(ccd_all)

#View the colnames to understand what it describes and plot accordingly
colnames(ccd_all)

## 2. Univariate Analysis on the dataframe

#Since Column 1. "ID" is unique to every value of the data. No need for univariate analysis and has no outliers
# 2.For Column - LIMIT_BAL 
boxplot(ccd_all$LIMIT_BAL)
hist(ccd_all$LIMIT_BAL, breaks = 10) # numeric - has outliers 
table(ccd_all$LIMIT_BAL) # need not to consider for outlier treatment

# 3.For Column - SEX 
boxplot(ccd_all$SEX)
hist(ccd_all$SEX) # Categorical - no outliers
table(ccd_all$SEX)

# 4. For Column - EDUCATION 
boxplot(ccd_all$EDUCATION)
hist(ccd_all$EDUCATION) # Categorical - there are three outliers - Need not to consider

# 5. For Column - MARRIAGE 
boxplot(ccd_all$MARRIAGE)
hist(ccd_all$MARRIAGE) # Categorical - has no outliers

# 6. For Column - AGE 
boxplot(ccd_all$AGE)
hist(ccd_all$AGE, breaks = 10) # Categorical - has outliers at upper quartile - Need not to consider

# 7. For Column - PAY_0 
boxplot(ccd_all$PAY_0)
hist(ccd_all$PAY_0, breaks = 20)# Categorical - has 7 outliers at upper quartile
table(ccd_all$PAY_0)# - Need not to consider

# 8. For Column - PAY_2
boxplot(ccd_all$PAY_2)
hist(ccd_all$PAY_2, breaks = 20) # Categorical - has 7 outliers at upper quartile 
table(ccd_all$PAY_2) # - Need not to consider
# 9. For column - PAY_3
boxplot(ccd_all$PAY_3)
hist(ccd_all$PAY_3, breaks = 20) # Categorical - has 7 outliers at upper quartile
table(ccd_all$PAY_3)# - Need not to consider
# 10. For Column - PAY_4
boxplot(ccd_all$PAY_4)
hist(ccd_all$PAY_4, breaks = 20) # Categorical - has 7 outliers at upper quartile
table(ccd_all$PAY_4)# - Need not to consider
# 11. For column - PAY_5
boxplot(ccd_all$PAY_5)
hist(ccd_all$PAY_5, breaks = 20) # Categorical - has 7 outliers at upper quartile
table(ccd_all$PAY_5)# - Need not to consider
# 12. For column - PAY_6
boxplot(ccd_all$PAY_6)
hist(ccd_all$PAY_6) # categorical - has 7 outliers at upper quartile
table(ccd_all$PAY_6)# - Need not to consider

# 13. For column - BILL_AMT1
boxplot(ccd_all$BILL_AMT1)
hist(ccd_all$BILL_AMT1, breaks = 30) 
# numeric - has outliers (at upper and lower quartiles) - Need to standardize the values

# 14. For column - BILL_AMT2
boxplot(ccd_all$BILL_AMT2)
hist(ccd_all$BILL_AMT2, breaks = 30) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 15. For column - BILL_AMT3
boxplot(ccd_all$BILL_AMT3)
hist(ccd_all$BILL_AMT3, breaks = 30) 
# numeric - has outliers (at upper and lower quartiles) - Need to standardize the values

# 16. For column - BILL_AMT4
boxplot(ccd_all$BILL_AMT4)
hist(ccd_all$BILL_AMT4, breaks = 30)
# numeric - has outliers (at upper and lower quartiles) - Need to standardize the values

# 17. For column - BILL_AMT5
boxplot(ccd_all$BILL_AMT5)
hist(ccd_all$BILL_AMT5, breaks = 30) 
# numeric - has outliers (at upper and lower quartiles) - Need to standardize the values

# 18. For column - BILL_AMT6
boxplot(ccd_all$BILL_AMT6)
hist(ccd_all$BILL_AMT6, breaks = 30) 
# numeric - has outliers (at upper and lower quartiles) - Need to standardize the values

# 19. For column - PAY_AMT1
boxplot(ccd_all$PAY_AMT1)
hist(ccd_all$PAY_AMT1, breaks = 10) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 20. For column - PAY_AMT2
boxplot(ccd_all$PAY_AMT2)
hist(ccd_all$PAY_AMT2, breaks = 10) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 21. For column - PAY_AMT3
boxplot(ccd_all$PAY_AMT3)
hist(ccd_all$PAY_AMT3, breaks = 10) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 22. For column - PAY_AMT4
boxplot(ccd_all$PAY_AMT4)
hist(ccd_all$PAY_AMT4, breaks = 10) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 23. For column - PAY_AMT5
boxplot(ccd_all$PAY_AMT5)
hist(ccd_all$PAY_AMT5, breaks = 10) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 24. For column - PAY_AMT6
boxplot(ccd_all$PAY_AMT6)
hist(ccd_all$PAY_AMT6, breaks = 10) 
# numeric - has outliers at upper quartile - Need to standardize the values

# 25. For column - default.payment.next.month
boxplot(ccd_all$default.payment.next.month)
hist(ccd_all$default.payment.next.month, breaks = 10) 
# Category - here the outlier to be considered and should not be removed

# 26. For column - default.payment.next.month
table(ccd_all$data) # Category

## 3.Data Visualization and Story Telling

## Plotting Single Categorical Variable

ggplot(ccd_all, aes(x = SEX))+geom_bar(color = "blue", fill = "white",width = 0.5)+
  xlab("Gender")+ylab("Frequency")+ggtitle("Gender Frequency")

#Here the take away is that most of the credit card holder are found to be in group 2 from the above plot

ggplot(ccd_all, aes(x = EDUCATION))+geom_bar(color = "pink", fill = "blue", width = 0.8)+
  xlab("Education")+ylab("Frequency")+ggtitle("Education Frequency")

# From the above plot, we can see that most of the education pursued by credit card holder are of group 2

ggplot(ccd_all, aes(x = MARRIAGE))+geom_bar(color = "green", fill = "purple", width = 0.3)+
  xlab("Marriage")+ylab("Frequency")+ggtitle("Marriage Frequency")

# From the above plot, we can see that most of the credit card holder are of group 2

ggplot(ccd_all, aes(x = AGE))+geom_bar(color = "pink", fill = "blue", width = 0.8)+
  xlab("Age")+ylab("Frequency")+ggtitle("Age Frequency")

# From the above plot, we can say that most of the card holders are of age 29

ggplot(ccd_all, aes(x = PAY_0))+geom_bar(color = "blue", fill = "white",width = 0.5)+
  xlab("First Payment")+ylab("Frequency")+ggtitle("First Payment Frequency")

# From the above plot , we can say that most of the card holders belongs to group 0

ggplot(ccd_all, aes(x = PAY_2))+geom_bar(color = "green", fill = "purple", width = 0.5)+
  xlab("Second Payment")+ylab("Frequency")+ggtitle("Second Payment Frequency")

# From the above plot , we can say that most of the card holders belongs to group 0

ggplot(ccd_all, aes(x = PAY_3))+geom_bar(color = "blue", fill = "white",width = 0.5)+
  xlab("Third Payment")+ylab("Frequency")+ggtitle("Third Payment Frequency")

# From the above plot , we can say that most of the card holders belongs to group 0

ggplot(ccd_all, aes(x = PAY_4))+geom_bar(color = "green", fill = "purple", width = 0.5)+
  xlab("Fourth Payment")+ylab("Frequency")+ggtitle("Fourth Payment Frequency")

# From the above plot , we can say that most of the card holders belongs to group 0

ggplot(ccd_all, aes(x = PAY_5))+geom_bar(color = "blue", fill = "white",width = 0.5)+
  xlab("Fifth Payment")+ylab("Frequency")+ggtitle("Fifth Payment Frequency")

# From the above plot , we can say that most of the card holders belongs to group 0

ggplot(ccd_all, aes(x = PAY_6))+geom_bar(color = "green", fill = "purple", width = 0.5)+
  xlab("Sixth Payment")+ylab("Frequency")+ggtitle("Sixth Payment Frequency")


## Plotting w.r.t Numerical and Categorical variables

ggplot(ccd_train, aes(x = factor(default.payment.next.month), y = LIMIT_BAL))+geom_boxplot()

ggplot(ccd_train, aes(x = factor(default.payment.next.month), y = LIMIT_BAL))+geom_boxplot()+coord_flip()

# Take way here is, card holders of group 0 holds Limit bal with 1,37,500/-
# and group 1 having limit bal with 1,12000/-

ggplot(ccd_train, aes(x = factor(SEX), y = LIMIT_BAL))+geom_boxplot()+coord_flip()

#group 1 of gender hold limit bal with 125000/- and group 2 hold limit bal with 1,35000/-

ggplot(ccd_train, aes(x = factor(EDUCATION), y = LIMIT_BAL))+geom_boxplot()+coord_flip()

# Here we can see that group 0 is having highest limit bal comparing to other groups of education

ggplot(ccd_train, aes(x = factor(MARRIAGE), y = LIMIT_BAL))+geom_boxplot()+coord_flip()

# From the above plot we can see that group 1 has been provided with highest limit bal compared to other groups of Marriage

## Plotting Single Numerical Variables - To verify the variability in the data looking at the plot

ggplot(ccd_all, aes(x = BILL_AMT1))+geom_histogram(bins = 100)

ggplot(ccd_all, aes(x = BILL_AMT1))+geom_density()

ggplot(ccd_all, aes(x = "BILL_AMT1",y = BILL_AMT1))+geom_violin()

ggplot(ccd_all, aes(x = "BILL_AMT1", y = BILL_AMT1))+geom_boxplot()

# Comparing density with normal
ggplot(ccd_all, aes(x = BILL_AMT1))+geom_density(color = "red")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(ccd_all$BILL_AMT1), sd = sd(ccd_all$BILL_AMT1)), col = "green")

## Plotting Numerical and Numerical Variables

ggplot(ccd_all, aes(x = BILL_AMT1, y = PAY_AMT1))+geom_point()

# Take away here is, there is no correlation between the bill_amt1 and pay_amt1

ggplot(ccd_all, aes(x = LIMIT_BAL, y = BILL_AMT1))+geom_point()+geom_smooth()

# There is a slight likely chance that as Limit bal increases there is an increase in the bill_amt1 variable

ggplot(ccd_all, aes(x = LIMIT_BAL, y = PAY_AMT1))+geom_point()+geom_smooth()

# There is a less likely chance that as limit bal increase there is a 
#less chance to pay the amount i.e pay_amt1(also Pay_amt2:pay_amt6)

## 4. Data Preparation - Missing data, Potentially incorrect data, Need for changing the form of data

# Structure fo the data
glimpse(ccd_all)

dim(ccd_all)

# Standardize Bill_amt and Pay_amt variables
standardize_values = function(x) {
  return((x - mean(x))/sd(x))
}

ccd_all$BILL_AMT1 = standardize_values(ccd_all$BILL_AMT1)
hist(ccd_all$BILL_AMT1)

ccd_all$BILL_AMT2 = standardize_values(ccd_all$BILL_AMT2)
hist(ccd_all$BILL_AMT2)

ccd_all$BILL_AMT3 = standardize_values(ccd_all$BILL_AMT3)
hist(ccd_all$BILL_AMT3)

ccd_all$BILL_AMT4 = standardize_values(ccd_all$BILL_AMT4)
hist(ccd_all$BILL_AMT4)

ccd_all$BILL_AMT5 = standardize_values(ccd_all$BILL_AMT5)
hist(ccd_all$BILL_AMT5)

ccd_all$BILL_AMT6 = standardize_values(ccd_all$BILL_AMT6)
hist(ccd_all$BILL_AMT6)

ccd_all$PAY_AMT1 = standardize_values(ccd_all$PAY_AMT1)
hist(ccd_all$PAY_AMT1)

ccd_all$PAY_AMT2 = standardize_values(ccd_all$PAY_AMT2)
hist(ccd_all$PAY_AMT2)

ccd_all$PAY_AMT3 = standardize_values(ccd_all$PAY_AMT3)
hist(ccd_all$PAY_AMT3)

ccd_all$PAY_AMT4 = standardize_values(ccd_all$PAY_AMT4)
hist(ccd_all$PAY_AMT4)

ccd_all$PAY_AMT5 = standardize_values(ccd_all$PAY_AMT5)
hist(ccd_all$PAY_AMT5)

ccd_all$PAY_AMT6 = standardize_values(ccd_all$PAY_AMT6)
hist(ccd_all$PAY_AMT6)

# All the above variables are standardized

#Verifying any missing values in the data

sapply(ccd_all, function(x) sum(is.na(x)))

glimpse(ccd_all)

#Verify the length of the Unique values in the table
sapply(ccd_all, function(x) length(unique(x)))

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

cat_cols = c("SEX", "EDUCATION","MARRIAGE", "AGE", "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6")

for (cat in cat_cols) {
  ccd_all = CreateDummies(ccd_all, cat, 50)
}

dim(ccd_all)

sum(sapply(ccd_all, function(x) is.character(x)))

table(ccd_all$default.payment.next.month)

## 5. Modelling Process  

#Separate dataset to begin Modelling process
ccd_train = ccd_all %>% filter(data == "train") %>% select(-data)
ccd_test = ccd_all %>% filter(data == "test") %>% select(-data, -default.payment.next.month)

set.seed(2)
s = sample(1: nrow(ccd_train), 0.8 * nrow(ccd_train))
ccd_train1 = ccd_train[s,]
ccd_train2 = ccd_train[-s,]

# To remove the multicollinearity between the variables and ensure all are independent variables using VIF
# A.) To Verify the VIF values of the predictor (VIF Cutoff is 10) 

library(car)

for_vif = lm(default.payment.next.month ~. -ID, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 1-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Interation 2-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 3-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 4-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 5-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 6-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 7-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 8-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 9-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT5, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 10-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT5-BILL_AMT2, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 11- 
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT5-BILL_AMT2-BILL_AMT3, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

# Final Iteration 
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT5-BILL_AMT2-BILL_AMT3-BILL_AMT4, data = ccd_train1)
sort(vif(for_vif), decreasing = T)[1:3]

#All VIF Values are less than 10

##6. Building Classification Model to estimate the tentative Performance

log_fit = glm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
              -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
              -PAY_6_0-BILL_AMT5-BILL_AMT2-BILL_AMT3-BILL_AMT4,
              data = ccd_train1, family = "binomial")

log_fit = step(log_fit)

summary(log_fit)

formula(log_fit)

log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                EDUCATION_3 + MARRIAGE_3 + MARRIAGE_1 + AGE_59 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_38 + AGE_39 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + AGE_29 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Run the above model and drop variables based on the p-values (cutoff is 0.05)

# Iteration 1-
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                EDUCATION_3 + MARRIAGE_3 + MARRIAGE_1 + AGE_59 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_38 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + AGE_29 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -2
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_3 + MARRIAGE_1 + AGE_59 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_38 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + AGE_29 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -3
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_3 + MARRIAGE_1 + AGE_59 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + AGE_29 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -4
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_3 + MARRIAGE_1 + AGE_59 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -5
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + PAY_AMT5 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_3 + MARRIAGE_1 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2+ 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -6
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_33 + AGE_34 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2+ 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -7
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_34 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2+ 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -8
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + AGE_58 + 
                AGE_45 + AGE_40 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2+ 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Iteration -9
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + 
                AGE_45 + AGE_40 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2+ 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

# Final Iteration
log_fit = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                PAY_AMT2 + SEX_2 + EDUCATION_4 + EDUCATION_5 + 
                MARRIAGE_1 + 
                AGE_45 + AGE_31 + 
                AGE_26 + PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + 
                PAY_2_3 + PAY_2__2 + PAY_2_2 + PAY_3_3 + PAY_3_2+ 
                PAY_4_3 + PAY_4_2 + PAY_4__1 + PAY_5_4 + PAY_5_2 + PAY_6_2 + 
                PAY_6__2 + PAY_6__1, data = ccd_train1, family = "binomial")

summary(log_fit)

## 7. Predict the model on the train2 data

library(pROC)

val.score = predict(log_fit, newdata = ccd_train2, type = "response")

auc_score = auc(roc(ccd_train2$default.payment.next.month, val.score))

auc_score

# So the tentative performance of Logistic Regression is going to be around 0.7641

#Visualizing through plot
mydata = data.frame( default.payment.next.month = ccd_train2$default.payment.next.month,
                     val.score = val.score)
ggplot(mydata, aes(x= val.score, y=default.payment.next.month,
                   color = factor(default.payment.next.month)))+geom_point()+geom_jitter()

# 7. Building the Model on entire Training Data
# VIF values of the predictor (VIF Cutoff is 10)
library(car)

for_vif = lm(default.payment.next.month ~. -ID, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 1-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 2-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 3-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 4-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 5-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 6-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 7-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 8-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 9-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT2, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 10-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT2-BILL_AMT5, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Iteration 11-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT2-BILL_AMT5-BILL_AMT3, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

#Final Iteration-
for_vif = lm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0
             -MARRIAGE_2-EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0
             -PAY_6_0-BILL_AMT2-BILL_AMT5-BILL_AMT3-BILL_AMT4, data = ccd_train)
sort(vif(for_vif), decreasing = T)[1:3]

## 8. Build model on Trained data

log.fit.final = glm(default.payment.next.month ~. -ID-PAY_5_0-PAY_4_0-MARRIAGE_2
                    -EDUCATION_2-PAY_3_0-PAY_0_0-PAY_2_0-PAY_6_0-BILL_AMT2-BILL_AMT5
                    -BILL_AMT3-BILL_AMT4, data = ccd_train, family = "binomial")

log.fit.final = step(log.fit.final)

formula(log.fit.final)

summary(log.fit.final)

# Run the above model and drop variables based on the p-values (cutoff is 0.05)

#Iteration-1
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT3 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_3 + MARRIAGE_1 + AGE_60 + 
                      AGE_58 + AGE_45 + AGE_38 + AGE_39 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 2-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT3 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_3 + MARRIAGE_1 + AGE_60 + 
                      AGE_45 + AGE_38 + AGE_39 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 3-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_3 + MARRIAGE_1 + AGE_60 + 
                      AGE_45 + AGE_38 + AGE_39 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 4-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + AGE_60 + 
                      AGE_45 + AGE_38 + AGE_39 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 5-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + MARRIAGE_1 + AGE_60 + 
                      AGE_45 + AGE_38 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 6-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + AGE_60 + 
                      AGE_45 + AGE_38 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 7-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + EDUCATION_3 + AGE_60 + 
                      AGE_45 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 8-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + AGE_60 + 
                      AGE_45 + AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 9-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + AGE_60 + 
                      AGE_34 + AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 10-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + AGE_60 + 
                      AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_5__1 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 11-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT5 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + AGE_60 + 
                      AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Iteration 12-
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + AGE_60 + 
                      AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_6_2 + PAY_6__2 + PAY_6__1,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

# Final Iteration 
log.fit.final = glm(default.payment.next.month ~ LIMIT_BAL + BILL_AMT1 + PAY_AMT1 + 
                      PAY_AMT2 + PAY_AMT6 + SEX_2 + EDUCATION_4 + 
                      EDUCATION_5 + AGE_60 + 
                      AGE_31 + AGE_26 + 
                      PAY_0_3 + PAY_0_2 + PAY_0_1 + PAY_0__1 + PAY_2_3 + PAY_2__2 + 
                      PAY_2_2 + PAY_3_3 + PAY_3_2 + PAY_4_4 + PAY_4_3 + PAY_4_2 + 
                      PAY_4__1 + PAY_5_2 + PAY_6_2 + PAY_6__2,
                    data = ccd_train, family ="binomial")
summary(log.fit.final)

#To submit simple probability score we can make prediction on test data and submit 
test.prob.score = predict(log.fit.final, newdata = ccd_test, type = 'response')
write.csv(test.prob.score, "credit_card_Payment_lr_test_prob_submission.csv", row.names = F)

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

#my_cutoff for F1 score is 0.749

#Convert predicted probability scores for the test to hardclasses -
test.predicted = as.numeric(test.prob.score > my_cutoff)
write.csv(test.predicted, "credit_card_holder_f1_ccd_lr_submission.csv",row.names=F)

##II.) For classification tree we'll need to convert response to factor type

is.numeric(ccd_all$default.payment.next.month)

ccd_all$default.payment.next.month = as.factor(ccd_all$default.payment.next.month)

#Separate dataset to begin Modelling process

ccd_train = ccd_all %>% filter(data == 'train') %>% select(-data)
ccd_test = ccd_all %>% filter(data == 'test') %>% select(-data, -default.payment.next.month)

set.seed(2)
s = sample(1 : nrow(ccd_train), 0.8 * nrow(ccd_train))
ccd_train1 = ccd_train[s,]
ccd_train2 = ccd_train[-s,]

## building tree 
library(tree)

#Building Classification Model to estimate the tentative Performance

ccd_tree_train1 = tree(default.payment.next.month ~. -ID, data=ccd_train1)
plot(ccd_tree_train1)
text(ccd_tree_train1)

# Predict the model on the train2 data

val.score = predict(ccd_tree_train1, newdata = ccd_train2, type = 'vector')[,1]
pROC::roc(ccd_train2$default.payment.next.month,val.score)$auc

#So the tentative performance on Decision Tree is : 0.7335

#Visualizing through plot
library(ggplot2)

mydata = data.frame( default.payment.next.month = ccd_train2$default.payment.next.month,
                     val.score = val.score)
ggplot(mydata, aes(x= val.score, y=default.payment.next.month,
                   color = factor(default.payment.next.month)))+geom_point()+geom_jitter()

# Build the model on the entire train data

ccd.tree.final = tree(default.payment.next.month ~. -ID, data = ccd_train)

#To submit simple probability score we can make prediction on test data and submit 

test.prob.score = predict(ccd.tree.final, newdata = ccd_test, type = 'vector')[,1]
write.csv(test.prob.score, "credit_card_holdertest_prob_dt_submission.csv", row.names = F)

# To Predict Hard classes instead of probabilities

train.score = predict(ccd.tree.final, newdata = ccd_train, type = "vector")[,1]
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
head(cutoff_data)
ggplot(cutoff_data, aes(x=cutoff, y=F.1))+geom_line()

# Visualize all of them at once
library(tidyr)
cutoff_long = cutoff_data %>% 
  gather(Measure, Value, Sn:M)

ggplot(cutoff_long, aes(x=cutoff, y=Value, color = Measure))+geom_line()

# Determined cutoff for F1-

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$F.1)]
my_cutoff

#my_cutoff for F1 score is 0.001

#Convert predicted probability scores for the test to hardclasses -
test.predicted = as.numeric(test.prob.score > my_cutoff)
write.csv(test.predicted, "credit_card_holder_f1_dt_submission.csv")

##III . Random Forest for Classification

## For classification tree we'll need to convert response to factor type
ccd_all$default.payment.next.month = as.factor(ccd_all$default.payment.next.month)

#Separate dataset to begin Modelling process
ccd_train = ccd_all %>%  filter(data == "train") %>% select(-data)
ccd_test = ccd_all %>% filter(data == "test") %>% select(-data, -default.payment.next.month)

#Model building on Trained data

library(randomForest)
library(cvTools)

subset_paras = function(full_list_para, n=10) {
  all_comb = expand.grid(full_list_para)
  s = sample(1 : nrow(all_comb), n)
  subset_para = all_comb[s,]
  return(subset_para)
}

param = list(mtry = c(5,10,15,20,25,35),
             ntree = c(50,100,200,500,700),
             maxnodes = c(5,10,15,20,30,50,100),
             nodesize = c(1,2,5,10))

mycost_auc = function(y,yhat) {
  roccurve = pROC::roc(y,yhat)
  score = pROC::auc(roccurve)
  return(score)
}

num_trails = 10
my_params = subset_paras(param, num_trails)

myauc=0

for(i in 1:num_trails){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,default.payment.next.month~. -ID, 
             data =ccd_train,
             tuning =params,
             folds = cvFolds(nrow(ccd_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

#Tentative performance of the measure

myauc

# my auc is 0.7412225

#The tentative performance measure -
# mtry ntree maxnodes nodesize
#    20   500      100        1

best_params

# Build the final model using train data
ccd.rf.final = randomForest(default.payment.next.month ~. -ID,mtry=best_params$mtry,
                            ntree=best_params$ntree,maxnodes=best_params$maxnodes,nodesize=best_params$nodesize,
                            data=ccd_train)

summary(ccd.rf.final)

#Use the model to predict for test data(while given) and submit as follows- 

test.predicted=predict(ccd.rf.final,newdata=ccd_test,type="prob")[,1]
write.csv(test.predicted, 'ccd_rf_test_prob_submission.csv', row.names=F)

# To Predict Hard classes instead of probabilities
train.score = predict(ccd.rf.final, newdata = ccd_train, type = "prob")[,1]
head(train.score)
real = ccd_train$default.payment.next.month
head(real)
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

head(cutoff_data)

library(ggplot2)
ggplot(cutoff_data, aes(x=cutoff, y=F.1))+geom_line()

# Visualize all of them at once
library(tidyr)
cutoff_long = cutoff_data %>% 
  gather(Measure, Value, Sn:M)

ggplot(cutoff_long, aes(x=cutoff, y=Value, color = Measure))+geom_line()

# Determined cutoff for F1-

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$F.1)]
my_cutoff

#my_cutoff for F1 score is 0.001

#Convert predicted probability scores for the test to hardclasses -
test.predicted = as.numeric(test.prob.score > my_cutoff)
write.csv(test.predicted, "credit_card_holder_f1_rf_submission.csv",row.names=F)

# IV.) Gradient Boosting Machine Algorithm for Classification

# For classification no need to convert to factor type for gbm
#we'll just change the distribution to "bernouli"

ccd_all$default.payment.next.month = as.numeric(ccd_all$default.payment.next.month==1)

#Separate dataset to begin Modelling process
ccd_train = ccd_all %>%  filter(data == "train") %>% select(-data)
ccd_test = ccd_all %>% filter(data == "test") %>% select(-data, -default.payment.next.month)

#Model building on trained data

library(gbm)
library(cvTools)
subset_paras = function(full_list_para, n=10) {
  all_comb = expand.grid(full_list_para)
  s = sample(1 : nrow(all_comb), n)
  subset_para = all_comb[s,]
  return(subset_para)
}

param = list(interaction.depth = c(1:7),
             n.trees = c(50,100,200,500,700),
             shrinkage = c(.1, .01, .001),
             n.minobsinnode= c(1,2,5,10))

num_trails = 10
my_params = subset_paras(param, num_trails)

mycost_auc = function(y,yhat) {
  roccurve = pROC::roc(y,yhat)
  score = pROC::auc(roccurve)
  return(score)
}

myauc = 0

for(i in 1:num_trails){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(gbm,default.payment.next.month~. -ID, 
             data =ccd_train,
             tuning =params,
             args = list(distribution = "bernoulli"),
             folds = cvFolds(nrow(ccd_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response", n.trees = params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}


myauc

#AUC score is 0.7788458

#Tentative performance measure
# interaction.depth n.trees shrinkage n.minobsinnode
#                 3     200       0.1              1

best_params

# Build the final model using train data

ccd.gbm.final = gbm(default.payment.next.month ~. -ID,
                    data = ccd_train,
                    n.trees = best_params$n.trees,
                    n.minobsinnode = best_params$n.minobsinnode,
                    shrinkage = best_params$shrinkage,
                    interaction.depth = best_params$interaction.depth,
                    distribution = "bernoulli")

summary(ccd.gbm.final)

#Use the model to predict for test data and submit as follows- 

test.score = predict(ccd.gbm.final, newdata = ccd_test, type = "response", n.trees = best_params$n.trees)
write.csv(test.score, "cp_ccd_gbm_test_prob_submission.csv", row.names = F)

# To Predict Hard classes instead of probabilities

train.score = predict(ccd.gbm.final, newdata = ccd_train, type = "response", n.trees = best_params$n.trees)
head(train.score)
real = ccd_train$default.payment.next.month
head(real)

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
head(cutoff_data)

library(ggplot2)
ggplot(cutoff_data, aes(x=cutoff, y=F.1))+geom_line()

# Visualize all of them at once
library(tidyr)
cutoff_long = cutoff_data %>% 
  gather(Measure, Value, Sn:M)

ggplot(cutoff_long, aes(x=cutoff, y=Value, color = Measure))+geom_line()

# Determined cutoff for F1-

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$F.1)]
my_cutoff

#my_cutoff for F1 score is 0.779

#Convert predicted probability scores for the test to hardclasses -
test.predicted = as.numeric(test.score > my_cutoff)
write.csv(test.predicted, "credit_card_holder_f1_gbm_submission.csv",row.names=F)
