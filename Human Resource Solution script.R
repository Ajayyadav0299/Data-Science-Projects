# get the working directory --------------------------------------------------------------------------------------------------------------------
getwd()

## set the working directory where to work and your data exists
setwd("C:\\Users\\HP 1\\Desktop\\data -projects\\")

## Data loading phase ---------------------------------------------------------------------------------------------------------------------------
## load the train and test data
train_data = read.csv("hr_train.csv", stringsAsFactors = F)
test_data = read.csv("hr_test.csv",stringsAsFactors = F)

## Data Preparation phase -----------------------------------------------------------------------------------------------------------------------
## combine data for data preparation
test_data$left = NA



# put place holder for train and test data for identification
train_data$data = 'train'
test_data$data = 'test'

# combining train and test data
All_data = rbind(train_data,test_data)


# loading library dplyer for data preparation
library(dplyr)

# view the data
glimpse(All_data)

## check all the column conating NA values
for(col in names(All_data)){
  if(sum(is.na(All_data[,col]))>0 & !(col %in% c("data","left")))
    print(col)
}



## finding names of the column which are of character type
char_logical=sapply(All_data, is.character)
cat_cols=names(All_data)[char_logical]
cat_cols


## finding names of the column which are of numeric type
char_logical1=sapply(All_data, is.numeric)
cat_cols1=names(All_data)[char_logical1]
cat_cols1

## taking only those columns which are categorical from cat_cols and cat_cols1
cat_cols = cat_cols[c(-3)]
cat_cols1 = cat_cols1[c(3,5)]


## function for creating dummies ---------------------------------------------------------------------------------------------------------
CreateDummies=function(data,var,freq_cutoff=100){
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
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

## creating dummy variable for all the categorical columns of character types
for(col in cat_cols){
  All_data=CreateDummies(All_data,col,50)
}

## creating dummy variable for all the categorical columns of numeric types
for(col in cat_cols1){
  All_data=CreateDummies(All_data,col,50)
}


## Sepeartion of data into train and test
train_data=All_data %>% filter(data=='train') %>% select(-data)
test_data= All_data %>% filter(data=='test') %>% select(-data,-left)




## seprate train and test data from train_data
set.seed(2)
v= sample(nrow(train_data), 0.80 * (nrow(train_data)))
training_data = train_data[v,]
testing_data = train_data[-v,]

## model making phase starts -----------------------------------------------------------------------------------------------------------------
## making linear model -----------------------------------------------------------------------------------------------------------------------
lin.fit = lm(left~.,data=training_data)

## finding aliased coefficents --------------------------------------------------------------------------------------------------------------
ld.vars <- attributes(alias(lin.fit)$Complete)$dimnames[[1]]
ld.vars

## use Vif for eliminating non contributing parameter-----------------------------------------------------------------------------------------
library(car)
sort(vif(lin.fit),decreasing = T)[1:3]


## eliminating vif value above 5 -------------------------------------------------------------------------------------------------------------
lin.fit = lm(left~.-time_spend_company_3,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(left~.-time_spend_company_3-number_project_4,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(left~.-time_spend_company_3-number_project_4-sales_sales,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]


## on the basis of vif model is as
formula(lin.fit)

## summarize
summary(lin.fit)

## now with the help of step function we remove variable which have probality greater than 0.05
log.fit = step(lin.fit)
summary(lin.fit)
formula(lin.fit)



## the final model is below ------------------------------------------------------------------------------------------------------------------
log.fit = glm(left ~ satisfaction_level + last_evaluation + average_montly_hours + 
                Work_accident  + sales_accounting + sales_RandD + 
                 salary_medium + salary_low + number_project_6 + 
                number_project_2 + number_project_5 + number_project_3 + 
                number_project_4 + time_spend_company_6 + time_spend_company_5 + 
                time_spend_company_4 + time_spend_company_3
              , data = training_data, family = "binomial")
summary(log.fit)

library(pROC)
## predict the score on testing data
predict.score=predict(log.fit,newdata = testing_data,type='response')
## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------
auc(roc(as.numeric(testing_data$left),as.numeric(predict.score)))
## score is very less so linear model cannot be fiited into the data very well -----------------------------------------------------------------


## making random forest model 
library(randomForest)
library(pROC)
rf.tree=randomForest(factor(left)~., data=training_data , do.trace = T, mtry = 4)
## predict the score on testing data
predict.score=predict(rf.tree,newdata = testing_data,type='prob')[,2]
## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------
auc(roc(as.numeric(testing_data$left),as.numeric(predict.score)))
## auc score is fine as it is ~ 0.85 so this is the best fittted model here --------------------------------------------------------------------


## let us build final model --------------------------------------------------------------------------------------------------------------------
final.model=randomForest(factor(left)~., data=train_data , do.trace = T, mtry = 4)
## predict the score on test data
final.predict=predict(rf.tree,newdata = test_data,type='prob')[,2]

## writing the results into csv files ----------------------------------------------------------------------------------------------------------
write.csv(final.predict,"Ajay_Yadav_P4_part2.csv",row.names = F)

