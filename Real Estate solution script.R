# get the working directory --------------------------------------------------------------------------------------------------------------------
getwd()

## set the working directory where to work and your data exists
setwd("C:\\Users\\HP 1\\Desktop\\Data projects\\")

## Data loading phase ---------------------------------------------------------------------------------------------------------------------------
## load the train and test data
train_data = read.csv("housing_train.csv", stringsAsFactors = F)
test_data = read.csv("housing_test.csv",stringsAsFactors = F)

## Data Preparation phase -----------------------------------------------------------------------------------------------------------------------
## combine data for data preparation
test_data$Price = NA

# put place holder for train and test data for identification
train_data$data = 'train'
test_data$data = 'test'

# combining train and test data
All_data = rbind(train_data,test_data)

# loading library dplyer for data preparation
library(dplyr)

# view the data
glimpse(All_data)


## finding names of the column which are of character type
char_logical=sapply(All_data, is.character)
cat_cols=names(All_data)[char_logical]
cat_cols


## finding names of the column which are of numeric categorical type 
char_logical1=sapply(All_data, is.numeric)
cat_cols1=names(All_data)[char_logical1]
cat_cols1

## omit all the NA values whose response is NA
All_data=All_data[!((is.na(All_data$Price)) & All_data$data=='train'), ]

## replacing NA values with median
for(col in names(All_data)){
  if(sum(is.na(All_data[,col]))>0 & !(col %in% c("data","Price"))){
    
    All_data[is.na(All_data[,col]),col]=median(All_data[,col],na.rm=T)
  }
}

# leaving the data column which is of characte type because this column of only for refrence of train and test data.
cat_cols = cat_cols[c(-7,-2)]
cat_cols1 = cat_cols1[c(1,4,5,6,7)]

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
  All_data=CreateDummies(All_data,col,100)
}

## creating dummy variable for all the categorical columns of numeric types
for(col in cat_cols1){
  All_data=CreateDummies(All_data,col,100)
}
# converting yearly built to no of years old
All_data$No_of_years_old = c(2018)- All_data$YearBuilt
All_data$YearBuilt = NULL
### Data preparation Phase over --------------------------------------------------------------------------------------------------------------

## Sepeartion of data into train and test
train_data=All_data %>% filter(data=='train') %>% select(-data)
test_data= All_data %>% filter(data=='test') %>% select(-data,-Price)


## seprate train and test data from train data
v= sample(nrow(train_data), 0.80 * (nrow(train_data)))
training_data = train_data[v,]
testing_data = train_data[-v,]

## model making phase starts -----------------------------------------------------------------------------------------------------------------
## making linear model -----------------------------------------------------------------------------------------------------------------------
lin.fit = lm(Price~.-Address,data=training_data)

## finding aliased coefficents --------------------------------------------------------------------------------------------------------------
ld.vars <- attributes(alias(lin.fit)$Complete)$dimnames[[1]]


## making of model after removal of alised coeffients ----------------------------------------------------------------------------------------

lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
             Postcode_3165- Postcode_3073,data=training_data)

## use Vif for eliminating non contributing parameter-----------------------------------------------------------------------------------------
library(car)
sort(vif(lin.fit),decreasing = T)[1:3]

## eliminating vif value above 5 -------------------------------------------------------------------------------------------------------------
lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165- Postcode_3073-Bedroom2_3,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165- Postcode_3073-Bedroom2_3-CouncilArea_ ,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165- Postcode_3073-Bedroom2_3-CouncilArea_ -Postcode_3121,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165- Postcode_3073-Bedroom2_3-CouncilArea_ -Postcode_3121-Rooms_2 ,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165- Postcode_3073-Bedroom2_3-CouncilArea_ -Postcode_3121-Rooms_2-Suburb_MalvernEast,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]
lin.fit = lm(Price~.-Address-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122
             -Postcode_3104-Postcode_3070-Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165- Postcode_3073-Bedroom2_3-CouncilArea_ -Postcode_3121-Rooms_2-Suburb_MalvernEast-Car_1-Bathroom_1-Postcode_3011-Postcode_3188,data=training_data)
sort(vif(lin.fit),decreasing = T)[1:3]

## on the basis of vif model is as
formula(lin.fit)

## summarize
summary(lin.fit)

## now with the help of step function we remove variable which have probality greater than 0.05
lin.fit = step(lin.fit)
summary(lin.fit)
formula(lin.fit)

## the final model is below ------------------------------------------------------------------------------------------------------------------
lin.fit = lm(Price ~ Distance + Landsize + BuildingArea + Suburb_Doncaster + 
               Suburb_Footscray + Suburb_Thornbury + Suburb_Hampton +  
               Suburb_Balwyn + Suburb_Camberwell + Suburb_PortMelbourne + 
               Suburb_PascoeVale + Suburb_BrightonEast + Suburb_Hawthorn + 
               Suburb_BalwynNorth +  Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + 
                Suburb_Preston +  Suburb_BentleighEast + 
               Suburb_Reservoir + Type_u + Type_h + Method_S + SellerG_Kay + 
               SellerG_Miles + SellerG_Greg + SellerG_RT + 
               SellerG_Buxton + SellerG_Marshall + SellerG_Jellis + CouncilArea_Whitehorse + 
               CouncilArea_Brimbank + CouncilArea_PortPhillip + CouncilArea_Yarra + 
               CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_MooneeValley + 
               CouncilArea_Moreland + CouncilArea_Boroondara + Rooms_1 + 
               Rooms_4 + Rooms_3 + Postcode_3147 + Postcode_3127 + Postcode_3081 + 
               Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3012 + 
               Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + 
               Postcode_3032 + Postcode_3046 + Postcode_3020 + Bedroom2_1 + 
               Bedroom2_2 + Car_3 + Car_0 + Car_2 + No_of_years_old, data = training_data)
summary(lin.fit)

## predict the score on testing data

test.predict = predict(lin.fit,newdata=testing_data)

## calculate RMSE
RMSE = (sum(testing_data$Price - test.predict))**2%>% mean() %>% sqrt()

## score
Score = 212467/RMSE
Score

## score is very less so linear model cannot be fiited into the data ---------------------------------------------------------------------------
## making random forest model 
library(randomForest)
set.seed(5)
rf.tree=randomForest(Price~.-Address,
                     cv = 10,ntree = 1000,
                     data=training_data )
rf.predict=predict(rf.tree,newdata = testing_data)

## RMSE(root mean square error) is calculated below:
rmse_val=((rf.predict)-(testing_data$Price))^2 %>% mean() %>% sqrt()
rmse_val

## score can be calculated using the formula below and the number 212467 is used to remove scale factor
Score = 212467/rmse_val
Score

## As we have calculated score on testing_data and it is fine and above 0.51 and a better score.
## build this model on full train_data and the below is final model -------------------------------------------------------------------------
set.seed(5)
final.model = randomForest(Price~.-Address,
                           cv = 10,ntree = 1000, data=train_data)

## predict the Price for the test_data that is for housing_test.csv

final.prediction = predict(final.model,newdata = test_data)


## write the final result into csv file -----------------------------------------------------------------------------------------------------

write.csv(final.prediction,"Ajay_Yadav_P1_part2.csv",row.names = F)









