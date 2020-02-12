#1.1
fileurl<-"https://raw.githubusercontent.com/AMULETAnalytics/UCLAIntroDataScience/master/homeworks/housing.csv"

#1.2
housing<-read.csv(fileurl)

#1.3
head(housing)

#  longitude latitude housing_median_age total_rooms total_bedrooms population households
#1   -122.23    37.88                 41         880            129        322        126
#2   -122.22    37.86                 21        7099           1106       2401       1138
#3   -122.24    37.85                 52        1467            190        496        177
#4   -122.25    37.85                 52        1274            235        558        219
#5   -122.25    37.85                 52        1627            280        565        259
#6   -122.25    37.85                 52         919            213        413        193
#2        8.3014             358500        NEAR BAY
#3        7.2574             352100        NEAR BAY
#4        5.6431             341300        NEAR BAY
#5        3.8462             342200        NEAR BAY
#6        4.0368             269700        NEAR BAY

#1.4

summary(housing)

#longitude         latitude     housing_median_age  total_rooms    total_bedrooms     population   
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0   Min.   :    3  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0   1st Qu.:  787  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0   Median : 1166  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9   Mean   : 1425  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0   3rd Qu.: 1725  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0   Max.   :35682  
#NA's   :207                     
#   households     median_income     median_house_value   ocean_proximity
# Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
# Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
# Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
 #3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :6082.0   Max.   :15.0001   Max.   :500001                      
                                                         
#2.1         

hist(housing$longitude, col = "blue")
hist(housing$latitude, col = "blue")
hist(housing$housing_median_age, col = "blue")
hist(housing$total_rooms, col = "blue")
hist(housing$total_bedrooms, col = "blue")
hist(housing$population, col = "blue")
hist(housing$households, col = "blue")
hist(housing$median_income, col = "blue")
hist(housing$median_house_value, col = "blue")

#2.2
# California population in that time has been young and included poeple mostly in their 30s and teenages
# Most of the population's income was around 3 and 4 dollars per hour
#Most of the house's value were around $240000

#3.1

install.packages("e1071")
library(e1071)    # Needed for impute()  
library(gtools)

housing_repaired<-impute(housing[,1:9], what="median")
housing_repaired = as.data.frame(housing_repaired)

blended<- cbind(housing[("ocean_proximity")], housing_repaired)

df.has.na <- apply(blended,1,function(x){any(is.na(x))})
sum(df.has.na)
#blended$`housing[, c("ocean_proximity")]`<-blended$"ocean_housing"
#final_housing<-smartbind(housing_repaired, housing[c("ocean_proximity")])


#3.2


oceans<-levels(blended$ocean_proximity)
oceans
binaryoceans<- function(c) {return(blended$ocean_proximity==c)}
newvars<-sapply(oceans, binaryoceans)

newvars
colnames(newvars)<-oceans
oceans

bin_ocean<-cbind(blended[,c("ocean_proximity")],newvars)
bin_ocean

final_housing<-cbind(blended, bin_ocean)
final_housing2<-final_housing[,-1]

df.has.na <- apply(final_housing2,1,function(x){any(is.na(x))})
sum(df.has.na)
#3.3

install.packages("dplyr")
library(dplyr)
final_housing2<-mutate(final_housing2, mean_number_bedrooms=total_bedrooms/households, mean_number_rooms=total_rooms/households)
final_housing2<-final_housing2[,-c(4:5)]
final_housing2<-final_housing2[,-8]
#3.4
ncol(final_housing2)

final_housing3<-scale(final_housing2[,1:6])
cleaned_housing<-cbind(final_housing3,final_housing2[,c(7:14)])


#3.5

colnames(cleaned_housing)

# [1] "longitude"            "latitude"             "housing_median_age"   "population"          
#[5] "households"           "median_income"        "median_house_value"   "<1H OCEAN"           
#[9] "INLAND"               "ISLAND"               "NEAR BAY"             "NEAR OCEAN"          
#[13] "mean_number_bedrooms" "mean_number_rooms"

#4.1

n<-nrow(cleaned_housing)
ntrain<-round(n*0.8)
set.seed(314)
tindex <- sample(n, ntrain)

trainhousing<-cleaned_housing[tindex,]
testhousing<-cleaned_housing[-tindex,]

#4.2

install.packages("randomForest")
library(randomForest)

train_x<-trainhousing[,-7]
train_y<-trainhousing[,7]

rf = randomForest(train_x, y = train_y ,
                  ntree = 500, importance = TRUE)

names(rf)
rf$importance

# [1] "call"            "type"            "predicted"       "mse"             "rsq"            
#[6] "oob.times"       "importance"      "importanceSD"    "localImportance" "proximity"      
#[11] "ntree"           "mtry"            "forest"          "coefs"           "y"              
#[16] "test"            "inbag"   

#                        %IncMSE IncNodePurity
#longitude            6586210016  2.500187e+13
#latitude             5365658476  2.175605e+13
#housing_median_age   1093570986  9.727319e+12
#population           1043209009  7.409226e+12
#households           1196887180  7.899785e+12
#median_income        8791886384  7.587905e+13
#<1H OCEAN            1844822760  4.929844e+12
#INLAND               4236244736  2.933543e+13
#ISLAND                  1943964  8.347689e+10
#NEAR BAY              492252977  1.413323e+12
#NEAR OCEAN            619666254  2.367178e+12
#mean_number_bedrooms  443243859  7.627145e+12
#mean_number_rooms    1839863219  2.071581e+13

#6

# Not specifying a data source forces OOB predictions
oob_prediction = predict(rf)
# Now compute the training set RMSE
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

# 48908.59

test_x<-testhousing[,-7]
test_y<-testhousing[,7]

y_pred = predict(rf , test_x)
# Now compute the test set RSME
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse

# 48568.47
#As you can see training set RSME is 48908.59 and test set RSME is 48568.47