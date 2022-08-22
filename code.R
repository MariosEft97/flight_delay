#######################################
##### STATISTICAL CONSULTING      #####
##### GROUP PROJECT               #####

#### Project contributors:        #####
#    Amorroti Sean - r0897919
#    Apostolou Konstantinos - r0864606
#    Efeti Martha - r0886202
#    Eftychiou Marios - r0868949
#
#######################################



rm(list=ls())

## install packages
packs<-c('lubridate','dplyr','tidyverse','magrittr','tidyr','randomForest','gdata')
install.packages(packs)

#load libraries
library(lubridate) # date object format
library(dplyr) # data binning
library(tidyverse)
library(magrittr) # pipes
library(tidyr) # drop_na
library(randomForest)
library(gdata)

# Functions

missing_values = function(dataset, df){
  
  for(i in 1:ncol(dataset)){
    col_names = colnames(dataset)
    na = sum(is.na(dataset[,i]))
    l = c(col_names[i], na)
    df[nrow(df) + 1,] = l
  }
  return(df)
}

performance<-function(tab){
  hitrate=round(sum(diag(tab)/sum(tab)),3)
  sensitivity=round(tab[2,2]/sum(tab[2,]),3)
  specificity=round(tab[1,1]/sum(tab[1,]),3)
  performance=c(hitrate=hitrate, sensitivity=sensitivity,specificity=specificity)
}

# Data are imported into RStudio.
filepath_1 = "C:\\Users\\35799\\Desktop\\KU Leuven\\Semester 2\\Statistical Consulting\\Group Project\\data_travel_agency\\airlines.csv"
filepath_2 = "C:\\Users\\35799\\Desktop\\KU Leuven\\Semester 2\\Statistical Consulting\\Group Project\\data_travel_agency\\airports.csv"
filepath_3 = "C:\\Users\\35799\\Desktop\\KU Leuven\\Semester 2\\Statistical Consulting\\Group Project\\data_travel_agency\\future_data.csv"
filepath_4 = "C:\\Users\\35799\\Desktop\\KU Leuven\\Semester 2\\Statistical Consulting\\Group Project\\data_travel_agency\\historic_data.csv"

##### my paths #####

fpaths=c("F:\\GENERAL\\LEUVEN\\KU LEUVEN\\Statistical Consulting\\data\\airlines.csv",
         "F:\\GENERAL\\LEUVEN\\KU LEUVEN\\Statistical Consulting\\data\\airports.csv",
         "F:\\GENERAL\\LEUVEN\\KU LEUVEN\\Statistical Consulting\\data\\future_data.csv",
         "F:\\GENERAL\\LEUVEN\\KU LEUVEN\\Statistical Consulting\\data\\historic_data.csv")
filepath_1=fpaths[1]
filepath_2=fpaths[2]
filepath_3=fpaths[3]
filepath_4=fpaths[4]

#########

airlines = read.csv(filepath_1, header=TRUE, sep = ",", na.strings = c("", " "))
airports = read.csv(filepath_2, header=TRUE, sep = ",", na.strings = c("", " "))
future_data = read.csv(filepath_3, header=TRUE, sep = ",", na.strings = c("", " "))
historic_data = read.csv(filepath_4, header=TRUE, sep = ",", na.strings = c("", " "))

head(airlines)
head(airports)
head(future_data)
head(historic_data)

# Dataset is divided into training and validation sets.
set.seed(12345)
d_test = sample(1:dim(historic_data)[1], round(dim(historic_data)[1]/2))
validation = historic_data[d_test, ]
train = historic_data[-d_test, ]

head(train)
head(validation)

###### TRAIN SET PRE-PROCESSING ######
# Summary of missing value per column of the training set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(train, df)

# Dealing with missing values for DEPARTURE_DELAY, ELAPSED_TIME and TAIL_NUMBER
train$DEPARTURE_DELAY[is.na(train$DEPARTURE_DELAY)] = median(train$DEPARTURE_DELAY, na.rm = T)
train$ELAPSED_TIME[is.na(train$ELAPSED_TIME)] = median(train$ELAPSED_TIME, na.rm = T)
train$SCHEDULED_TIME[is.na(train$SCHEDULED_TIME)] = median(train$SCHEDULED_TIME, na.rm = T)
train=train[!is.na(train$TAIL_NUMBER),]

# columns to drop
drop = c("DEPARTURE_TIME", "ARRIVAL_TIME",
         "TAXI_OUT", "WHEELS_OFF","AIR_TIME",
         "WHEELS_ON", "TAXI_IN", "CANCELLATION_REASON",
         "AIR_SYSTEM_DELAY", "SECURITY_DELAY",
         "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY",
         "WEATHER_DELAY")

train_processed = train[, !names(train)%in%drop]

# Summary of missing value per column of the training set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(train_processed, df)

head(train_processed)

# Compute arrival delay: DEPARTURE_DELAY - (SCHEDULED_TIME-ELAPSED_TIME)
# Positive arrival delay means that the flight was delayed in arrival
# Negative arrival delay means that the flight was early in arrival
# 0 arrival delay means that the flight was on-time in arrival
# NA arrival delay means that the flight was cancelled
train_processed["DELAY"] = train_processed$DEPARTURE_DELAY - (train_processed$SCHEDULED_TIME-train_processed$ELAPSED_TIME)
#train_processed["DELAY"][train_processed["CANCELLED"]==1]=NA

# Date variables processing
train_processed["MONTH"] = month(train_processed$SCHEDULED_DEPARTURE)
train_processed["MONTH_DAY"] = mday(train_processed$SCHEDULED_DEPARTURE)
train_processed["WEEK_DAY"] = wday(train_processed$SCHEDULED_DEPARTURE, week_start = getOption("lubridate.week.start", 1))
train_processed["HOUR"] = hour(train_processed$SCHEDULED_DEPARTURE)

drop = c("SCHEDULED_DEPARTURE","SCHEDULED_ARRIVAL","DEPARTURE_DELAY", "SCHEDULED_TIME", "ELAPSED_TIME")
train_processed = train_processed[, !names(train_processed)%in%drop]

head(train_processed)

# Summary of missing value per column of the training set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(train_processed, df)

# Combining train data with origin airport information
train_processed_merged = merge(train_processed, airports, by.x="ORIGIN_AIRPORT", by.y="IATA_CODE", all.x = TRUE, all.y = FALSE)
#train_processed_merged["ORIGIN_AIRPORT"] = train_processed_merged["AIRPORT"]
train_processed_merged["ORIGIN_CITY"] = train_processed_merged["CITY"]
train_processed_merged["ORIGIN_STATE"] = train_processed_merged["STATE"]

# columns to drop
drop = c("AIRPORT", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE")
train_processed_merged = train_processed_merged[, !names(train_processed_merged)%in%drop]

# Combining train data with destination airport information
train_processed_merged = merge(train_processed_merged, airports, by.x="DESTINATION_AIRPORT", by.y="IATA_CODE", all.x = TRUE, all.y = FALSE)
#train_processed_merged["DESTINATION_AIRPORT"] = train_processed_merged["AIRPORT"]
train_processed_merged["DESTINATION_CITY"] = train_processed_merged["CITY"]
train_processed_merged["DESTINATION_STATE"] = train_processed_merged["STATE"]

# columns to drop
drop = c("AIRPORT", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE")
train_processed_merged = train_processed_merged[, !names(train_processed_merged)%in%drop]
head(train_processed_merged)

# reorder columns
col_order = c("MONTH", "MONTH_DAY", "WEEK_DAY", "HOUR", "AIRLINE","FLIGHT_NUMBER", "TAIL_NUMBER", "ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE", "DESTINATION_AIRPORT", "DESTINATION_CITY", "DESTINATION_STATE", "DISTANCE", "CANCELLED", "DELAY")

train_final = train_processed_merged[, col_order]
head(train_final)

# Binning

# MONTH_DAY   BIN
# 1-7         1
# 8-15        2
# 16-23       3
# 23-31       4

# WEEK_DAY                BIN
# Monday (1)              1
# Tuesday-Thursday (2-4)  2
# Friday (5)              3
# Saturday-Sunday(6-7)    4

# HOUR    BIN
# 0-5     1
# 6-11    2
# 12-17   3
# 18-23   4

train_binned = train_final

train_binned$MONTHDAY_BIN = cut(train_binned$MONTH_DAY, breaks=c(0, 8, 16, 24, 32), labels=FALSE)
train_binned$WEEKDAY_BIN = cut(train_binned$WEEK_DAY, breaks=c(0, 1, 4, 5, 8), labels=FALSE)
train_binned$HOUR_BIN = cut(train_binned$HOUR, breaks=c(-1, 5, 11, 17, 24), labels=FALSE)

# reorder columns
col_order = c("MONTH", "MONTH_DAY", "WEEK_DAY", "HOUR", "MONTHDAY_BIN", "WEEKDAY_BIN", "HOUR_BIN","AIRLINE", "FLIGHT_NUMBER", "TAIL_NUMBER",
              "ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE","DESTINATION_AIRPORT", "DESTINATION_CITY", "DESTINATION_STATE",
              "CANCELLED", "DELAY", "DISTANCE")

train_binned = train_binned[, col_order]

train_temporary = train_binned
train_temporary["DELAY"][train_temporary["CANCELLED"]==1]=NA

train_set = train_temporary
train_set["DELAYED"] = ifelse(train_set["DELAY"]>=15|is.na(train_set["DELAY"])==TRUE, 1,0)
drop = c("CANCELLED","DELAY", "MONTH_DAY", "WEEK_DAY", "HOUR")
train_set_final = train_set[, !names(train_set)%in%drop]

###### VALIDATION SET PRE-PROCESSING ######
# Summary of missing value per column of the training set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(validation, df)

# Dealing with missing values for DEPARTURE_DELAY, ELAPSED_TIME and TAIL_NUMBER
validation$DEPARTURE_DELAY[is.na(validation$DEPARTURE_DELAY)] = median(train$DEPARTURE_DELAY, na.rm = T)
validation$ELAPSED_TIME[is.na(validation$ELAPSED_TIME)] = median(train$ELAPSED_TIME, na.rm = T)
validation=validation[!is.na(validation$TAIL_NUMBER),]

# columns to drop
drop = c("DEPARTURE_TIME", "ARRIVAL_TIME",
         "TAXI_OUT", "WHEELS_OFF","AIR_TIME",
         "WHEELS_ON", "TAXI_IN", "CANCELLATION_REASON",
         "AIR_SYSTEM_DELAY", "SECURITY_DELAY",
         "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY",
         "WEATHER_DELAY")

validation_processed = validation[, !names(validation)%in%drop]

# Summary of missing value per column of the training set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(validation_processed, df)

head(validation_processed)

# Compute arrival delay: DEPARTURE_DELAY - (SCHEDULED_TIME-ELAPSED_TIME)
# Positive arrival delay means that the flight was delayed in arrival
# Negative arrival delay means that the flight was early in arrival
# 0 arrival delay means that the flight was on-time in arrival
# NA arrival delay means that the flight was cancelled
validation_processed["DELAY"] = validation_processed$DEPARTURE_DELAY - (validation_processed$SCHEDULED_TIME-validation_processed$ELAPSED_TIME)
#train_processed["DELAY"][train_processed["CANCELLED"]==1]=NA

# Date variables processing
validation_processed["MONTH"] = month(validation_processed$SCHEDULED_DEPARTURE)
validation_processed["MONTH_DAY"] = mday(validation_processed$SCHEDULED_DEPARTURE)
validation_processed["WEEK_DAY"] = wday(validation_processed$SCHEDULED_DEPARTURE, week_start = getOption("lubridate.week.start", 1))
validation_processed["HOUR"] = hour(validation_processed$SCHEDULED_DEPARTURE)

drop = c("SCHEDULED_DEPARTURE","SCHEDULED_ARRIVAL","DEPARTURE_DELAY", "SCHEDULED_TIME", "ELAPSED_TIME")
validation_processed = validation_processed[, !names(validation_processed)%in%drop]

head(validation_processed)

# Combining train data with origin airport information
validation_processed_merged = merge(validation_processed, airports, by.x="ORIGIN_AIRPORT", by.y="IATA_CODE", all.x = TRUE, all.y = FALSE)
#validation_processed_merged["ORIGIN_AIRPORT"] = validation_processed_merged["AIRPORT"]
validation_processed_merged["ORIGIN_CITY"] = validation_processed_merged["CITY"]
validation_processed_merged["ORIGIN_STATE"] = validation_processed_merged["STATE"]

# columns to drop
drop = c("AIRPORT", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE")
validation_processed_merged = validation_processed_merged[, !names(validation_processed_merged)%in%drop]

# Combining train data with destination airport information
validation_processed_merged = merge(validation_processed_merged, airports, by.x="DESTINATION_AIRPORT", by.y="IATA_CODE", all.x = TRUE, all.y = FALSE)
#validation_processed_merged["DESTINATION_AIRPORT"] = validation_processed_merged["AIRPORT"]
validation_processed_merged["DESTINATION_CITY"] = validation_processed_merged["CITY"]
validation_processed_merged["DESTINATION_STATE"] = validation_processed_merged["STATE"]

# columns to drop
drop = c("AIRPORT", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE")
validation_processed_merged = validation_processed_merged[, !names(validation_processed_merged)%in%drop]
head(validation_processed_merged)
# reorder columns
col_order = c("MONTH", "MONTH_DAY", "WEEK_DAY", "HOUR", "AIRLINE","FLIGHT_NUMBER", "TAIL_NUMBER", "ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE", "DESTINATION_AIRPORT", "DESTINATION_CITY", "DESTINATION_STATE", "DISTANCE", "CANCELLED", "DELAY")

validation_final = validation_processed_merged[, col_order]
head(validation_final)

# Binning

# MONTH_DAY   BIN
# 1-7         1
# 8-15        2
# 16-23       3
# 23-31       4

# WEEK_DAY                BIN
# Monday (1)              1
# Tuesday-Thursday (2-4)  2
# Friday (5)              3
# Saturday-Sunday(6-7)    4

# HOUR    BIN
# 0-5     1
# 6-11    2
# 12-17   3
# 18-23   4

validation_binned = validation_final

validation_binned$MONTHDAY_BIN = cut(validation_binned$MONTH_DAY, breaks=c(0, 8, 16, 24, 32), labels=FALSE)
validation_binned$WEEKDAY_BIN = cut(validation_binned$WEEK_DAY, breaks=c(0, 1, 4, 5, 8), labels=FALSE)
validation_binned$HOUR_BIN = cut(validation_binned$HOUR, breaks=c(-1, 5, 11, 17, 24), labels=FALSE)

# reorder columns
col_order = c("MONTH", "MONTH_DAY", "WEEK_DAY", "HOUR", "MONTHDAY_BIN", "WEEKDAY_BIN", "HOUR_BIN","AIRLINE", "FLIGHT_NUMBER", "TAIL_NUMBER",
              "ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE","DESTINATION_AIRPORT", "DESTINATION_CITY", "DESTINATION_STATE",
              "CANCELLED", "DELAY", "DISTANCE")

validation_binned = validation_binned[, col_order]

validation_temporary = validation_binned
validation_temporary["DELAY"][validation_temporary["CANCELLED"]==1]=NA

validation_set = validation_temporary
validation_set["DELAYED"] = ifelse(validation_set["DELAY"]>=15|is.na(validation_set["DELAY"])==TRUE, 1,0)
drop = c("CANCELLED","DELAY", "MONTH_DAY", "WEEK_DAY", "HOUR")
validation_set_final = validation_set[, !names(validation_set)%in%drop]
head(validation_set_final)

###### TEST SET PRE-PROCESSING ######
# Summary of missing value per column of the test set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(future_data, df)

# Dealing with missing values for DEPARTURE_DELAY, ELAPSED_TIME and TAIL_NUMBER
test=future_data[!is.na(future_data$TAIL_NUMBER),]

# columns to drop
drop = c("DEPARTURE_TIME", "ARRIVAL_TIME",
         "TAXI_OUT", "WHEELS_OFF","AIR_TIME",
         "WHEELS_ON", "TAXI_IN", "CANCELLATION_REASON",
         "AIR_SYSTEM_DELAY", "SECURITY_DELAY",
         "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY",
         "WEATHER_DELAY")

test_processed = test[, !names(test)%in%drop]

# Summary of missing value per column of the training set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(test_processed, df)

# Date variables processing
test_processed$SCHEDULED_DEPARTURE = ymd_hms(test_processed$SCHEDULED_DEPARTURE, tz="UTC")
test_processed["MONTH"] = month(test_processed$SCHEDULED_DEPARTURE)
test_processed["MONTH_DAY"] = mday(test_processed$SCHEDULED_DEPARTURE)
test_processed["WEEK_DAY"] = wday(test_processed$SCHEDULED_DEPARTURE, week_start = getOption("lubridate.week.start", 1))
test_processed["HOUR"] = hour(test_processed$SCHEDULED_DEPARTURE)

drop = c("SCHEDULED_DEPARTURE","SCHEDULED_ARRIVAL","DEPARTURE_DELAY", "SCHEDULED_TIME", "ELAPSED_TIME")
test_processed = test_processed[, !names(test_processed)%in%drop]

head(test_processed)

# Summary of missing value per column of the test set
df = data.frame("Column"=character(), "NA number"=character())
missing_values(test_processed, df)

# reorder columns
col_order = c("MONTH", "MONTH_DAY", "WEEK_DAY", "HOUR", "AIRLINE","FLIGHT_NUMBER", "TAIL_NUMBER", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "DISTANCE")

test_final = test_processed[, col_order]
head(test_final)

# Binning

# MONTH_DAY   BIN
# 1-7         1
# 8-15        2
# 16-23       3
# 23-31       4

# WEEK_DAY                BIN
# Monday (1)              1
# Tuesday-Thursday (2-4)  2
# Friday (5)              3
# Saturday-Sunday(6-7)    4

# HOUR    BIN
# 0-5     1
# 6-11    2
# 12-17   3
# 18-23   4

test_binned = test_final

test_binned$MONTHDAY_BIN = cut(test_binned$MONTH_DAY, breaks=c(0, 8, 16, 24, 32), labels=FALSE)
test_binned$WEEKDAY_BIN = cut(test_binned$WEEK_DAY, breaks=c(0, 1, 4, 5, 8), labels=FALSE)
test_binned$HOUR_BIN = cut(test_binned$HOUR, breaks=c(-1, 5, 11, 17, 24), labels=FALSE)

# reorder columns
col_order = c("MONTH", "MONTH_DAY", "WEEK_DAY", "HOUR", "MONTHDAY_BIN", "WEEKDAY_BIN", "HOUR_BIN","AIRLINE", "FLIGHT_NUMBER", "TAIL_NUMBER",
              "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "DISTANCE")

test_binned = test_binned[, col_order]

test_binned[,"DELAYED"]=0
drop = c("MONTH_DAY", "WEEK_DAY", "HOUR")
test_set_final = test_binned[, !names(test_binned)%in%drop]
head(test_set_final)


#### Exploratory Data Analysis ####
head(train_final)

# Correlation between variables
round(cor(train_final[c("DELAY", "MONTH_DAY")]), 3)
round(cor(train_final[c("DELAY", "WEEK_DAY")]), 3)
round(cor(train_final[c("DELAY", "HOUR")]), 3)
round(cor(train_final[c("DELAY", "DISTANCE")]), 3)


df_plots = merge(train_temporary, airlines, by.x="AIRLINE", by.y="IATA_CODE", all.x = TRUE, all.y = FALSE)
head(df_plots)
df = data.frame("Column"=character(), "NA number"=character())
missing_values(df_plots, df)

# Boxplots of AIRLINE, ORIGIN_AIRPORT and DESTINATION_AIRPORT
ggplot(df_plots, aes(x=DELAY, y=AIRLINE.y)) +  geom_violin(fill='blue') +title('Distribution of Delay amount per Airline')

ggsave('violin_plot.png',device='png',width=5,height=4,dpi=700)
#ggplot(train_final, aes(x=DELAY, y=ORIGIN_AIRPORT)) +  geom_boxplot(fill='gray')
#ggplot(train_final, aes(x=DELAY, y=DESTINATION_AIRPORT)) +  geom_boxplot(fill='gray')

# #Plot flight delays group by airlines
# ggplot(train_final, aes(x=AIRLINE, y = DELAY, col = AIRLINE)) +
#   geom_jitter(alpha = 0.5, size = 0.3) +
#   coord_flip() +
#   theme(legend.position = "none") +
#   xlab("Airlines") +
#   ylab("Delay (in minutes)")

#Number of flights by different airlines
df_plots %>% mutate(delay_group = case_when(DELAY<=15 ~ "on-time", DELAY>15 ~ "delayed",is.na(DELAY) == TRUE ~ "cancelled")) %>%
  ggplot(aes(x = AIRLINE.y, fill = delay_group)) +
  geom_bar(stat = "count", position = "dodge") +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("on-time" = "green", "delayed" = "yellow", "cancelled" = "red")) +
  xlab("Airlines") +
  ylab("Flight Count")

ggsave('bar_plot.png',device='png',width=5,height=4,dpi=700)

#distribution of flight delays and cancellation group by days of month
train_set %>% filter(DELAYED == 1) %>% group_by(MONTH_DAY) %>% summarize(n_delays = n()) %>%
  ggplot(aes(x= MONTH_DAY, y = n_delays)) +
  geom_point() +
  geom_line(col = "blue") +
  scale_x_discrete(limits = factor(1:31))

ggsave('month_day.png',device='png',width=5,height=4,dpi=700)


#distribution of flight delays and cancellation group by days of week
train_set %>% filter(DELAYED == 1) %>% group_by(WEEK_DAY) %>% summarize(n_delays = n()) %>%
  ggplot(aes(x= WEEK_DAY, y = n_delays)) +
  geom_point() +
  geom_line(col = "blue") +
  scale_x_discrete(limits = factor(1:7)) +
  ylab('Delayed Flights')+
  xlab('Day of the Week')+
  title('Distribution of Delayed Flights by Day of the Week')

ggsave('week_day.png',device='png',width=5,height=4,dpi=700)


#distribution of flight delays and cancellations by the day of the week
train_set %>% filter(DELAYED == 1) %>% group_by(WEEK_DAY) %>% summarize(n_delays = n()) %>%
  ggplot(aes(x= WEEK_DAY, y = n_delays)) +
  geom_col()

#flight delay group by hour in a day
train_set %>% filter(DELAYED == 1) %>% group_by(HOUR) %>% summarize(n_delays = n()) %>%
  ggplot(aes(x= HOUR, y = n_delays)) +
  geom_point() +
  geom_line(col = "blue")

ggsave('hour_day.png',device='png',width=5,height=4,dpi=700)


ggplot(df_plots) +
  geom_point(mapping = aes(x=MONTH, y = DELAY, color = AIRLINE.y)) +
  facet_wrap(~AIRLINE.y, nrow=2) +
  labs(title = "Delay by Airline by Month",
       x = "Month",
       y = "Delay",
       colour = "Airline")

ggsave('delay_airline.png',device='png',width=5,height=4,dpi=700)

#### Algorithm training ####

# keep<-c(missing_values,performance,train_set_final,validation_set_final)
# keep(missing_values,performance,train_set_final,validation_set_final,sure=T) ## keep only the necessary datasets to help clear memory space
# gc()

head(train_set_final)
dim(train_set_final)
table(train_set_final$DELAYED)
df = data.frame("Column"=character(), "NA number"=character())
missing_values(train_set_final, df)

### Take away the columns: ORIGIN_CITY, ORIGIN_STATE, DESTINATION_CITY, ORIGIN_STATE ??? ###
train_set_final<-train_set_final[,-c(9,10,12,13)]
n<-dim(train_set_final)[2]-1
l<-dim(train_set_final)[1]

validation_set_final<-validation_set_final[,-c(9,10,12,13)]


## for fitting the model, we elect to use a subset of the train dataset with less independent variables.

#### Bagging ####
set.seed(12345)
bagging_tree=randomForest(as.factor(train_set_final$DELAYED)~.,data=train_set_final, mtry=n, ntree=40,importance=TRUE)
bagging_tree
round(importance(bagging_tree),3)
varImpPlot(bagging_tree, sort=TRUE, n.var=n)

plot(bagging_tree)

# The Mean Decrease Accuracy plot expresses how much accuracy the model losses by excluding each variable.
# The more the accuracy suffers, the more important the variable is for the successful classification.
# The variables are presented from descending importance. The mean decrease in Gini coefficient is a measure
# of how each variable contributes to the homogeneity of the nodes and leaves in the resulting random forest.
# The higher the value of mean decrease accuracy or mean decrease Gini score, the higher the importance of the
# variable in the model.

#predictions training data
pred_bagging_train=predict(bagging_tree,newdata=train_set_final,type="prob")

#Bayes classifier
class_train_equal=ifelse(pred_bagging_train[,1]>0.5,0,1)
tab_bagging_cost_equal_train = table(train_set_final$DELAYED,class_train_equal)
bagging_cost_equal_performance_train = performance(tab_bagging_cost_equal_train)
bagging_cost_equal_performance_train

#unequal classification costs
class_train_unequal=ifelse(pred_bagging_train[,1]>=pred_bagging_train[,2]*1.5,0,1)
tab_bagging_cost_unequal_train = table(train_set_final$DELAYED,class_train_unequal)
bagging_cost_unequal_performance_train = performance(tab_bagging_cost_unequal_train)
bagging_cost_unequal_performance_train

#predictions validation data
head(validation_set_final)
dim(validation_set_final)
table(validation_set_final$DELAYED)

pred_bagging_validation=predict(bagging_tree,newdata=validation_set_final,type="prob")

#Bayes classifier
class_validation_equal=ifelse(pred_bagging_validation[,1]>0.5,0,1)
tab_bagging_cost_equal_validation = table(validation_set_final$DELAYED,class_validation_equal)
bagging_cost_equal_performance_validation = performance(tab_bagging_cost_equal_validation)
bagging_cost_equal_performance_validation

#unequal classification costs
class_validation_unequal=ifelse(pred_bagging_validation[,1]>=pred_bagging_validation[,2]*1.5,0,1)
tab_bagging_cost_unequal_validation = table(validation_set_final$DELAYED, class_validation_unequal)
bagging_cost_unequal_performance_validation = performance(tab_bagging_cost_unequal_validation)
bagging_cost_unequal_performance_validation



##### Random Forest #####
random_forest_tree=randomForest(as.factor(train_set_final$DELAYED)~.,data=train_set_final, mtry=4, ntree=40,importance=TRUE)
random_forest_tree
round(importance(random_forest_tree),3)
varImpPlot(random_forest_tree, sort=TRUE, n.var=n)

plot(random_forest_tree)


#predictions training data
pred_rf_train=predict(random_forest_tree,newdata=train_set_final,type="prob")

#Bayes classifier
class_train_equal=ifelse(pred_rf_train[,1]>0.5,0,1)
tab_rf_cost_equal_train = table(train_set_final$DELAYED,class_train_equal)
rf_cost_equal_performance_train = performance(tab_rf_cost_equal_train)
rf_cost_equal_performance_train

#unequal classification costs
class_train_unequal=ifelse(pred_rf_train[,1]>=pred_rf_train[,2]*1.5,0,1)
tab_rf_cost_unequal_train = table(train_set_final$DELAYED,class_train_unequal)
rf_cost_unequal_performance_train = performance(tab_rf_cost_unequal_train)
rf_cost_unequal_performance_train

#predictions validation data
head(validation_set_final)
dim(validation_set_final)
table(validation_set_final$DELAYED)

pred_rf_validation=predict(random_forest_tree,newdata=validation_set_final,type="prob")

#Bayes classifier
class_validation_equal=ifelse(pred_rf_validation[,1]>0.5,0,1)
tab_rf_cost_equal_validation = table(validation_set_final$DELAYED,class_validation_equal)
rf_cost_equal_performance_validation = performance(tab_rf_cost_equal_validation)
rf_cost_equal_performance_validation

#unequal classification costs
class_validation_unequal=ifelse(pred_rf_validation[,1]>=pred_rf_validation[,2]*1.5,0,1)
tab_rf_cost_unequal_validation = table(validation_set_final$DELAYED, class_validation_unequal)
rf_cost_unequal_performance_validation = performance(tab_rf_cost_unequal_validation)
rf_cost_unequal_performance_validation



#### Undersampling ####
# Because of the unbalanced dataset the algorithm is unable to learn correctly.
# Thus we have biased predictions and misleading results.

# train set
df_ontime = which(train_set_final$DELAYED == 0)
df_delayed = which(train_set_final$DELAYED == 1)
nsample = table(train_set_final$DELAYED)[2]*1.7
pick = sample(df_ontime, nsample)
train_balanced = train_set_final[c(df_delayed, pick), ]

dim(train_balanced)
table(train_balanced$DELAYED)
head(train_balanced)

# validation set
df_ontime = which(validation_set_final$DELAYED == 0)
df_delayed = which(validation_set_final$DELAYED == 1)

### setting negative counts to be same as positive counts - so that the data is balanced
nsample = table(validation_set_final$DELAYED)[2]*1.7
pick = sample(df_ontime, nsample)
validation_balanced = validation_set_final[c(df_delayed, pick), ]

dim(validation_balanced)
table(validation_balanced$DELAYED)
head(validation_balanced)

n<-dim(train_balanced)[2]-1
l<-dim(train_balanced)[1]

# bagging
bagging_tree=randomForest(as.factor(train_balanced$DELAYED)~.,data=train_balanced, mtry=n,ntree=50,importance=TRUE)
bagging_tree
round(importance(bagging_tree),3)
varImpPlot(bagging_tree, sort=TRUE, n.var=n)

# The Mean Decrease Accuracy plot expresses how much accuracy the model losses by excluding each variable.
# The more the accuracy suffers, the more important the variable is for the successful classification.
# The variables are presented from descending importance. The mean decrease in Gini coefficient is a measure
# of how each variable contributes to the homogeneity of the nodes and leaves in the resulting random forest.
# The higher the value of mean decrease accuracy or mean decrease Gini score, the higher the importance of the
# variable in the model.

#predictions training data
pred_bagging_train=predict(bagging_tree,newdata=train_balanced,type="prob")

#Bayes classifier
class_train_equal=ifelse(pred_bagging_train[,1]>0.5,0,1)
tab_bagging_cost_equal_train = table(train_balanced$DELAYED,class_train_equal)
bagging_cost_equal_performance_train = performance(tab_bagging_cost_equal_train)
bagging_cost_equal_performance_train

#unequal classification costs
class_train_unequal=ifelse(pred_bagging_train[,1]>=pred_bagging_train[,2]*1.5,0,1)
tab_bagging_cost_unequal_train = table(train_balanced$DELAYED,class_train_unequal)
bagging_cost_unequal_performance_train = performance(tab_bagging_cost_unequal_train)
bagging_cost_unequal_performance_train

#predictions validation data
pred_bagging_validation=predict(bagging_tree,newdata=validation_balanced,type="prob")

#Bayes classifier
class_validation_equal=ifelse(pred_bagging_validation[,1]>0.5,0,1)
tab_bagging_cost_equal_validation = table(validation_balanced$DELAYED,class_validation_equal)
bagging_cost_equal_performance_validation = performance(tab_bagging_cost_equal_validation)
bagging_cost_equal_performance_validation

#unequal classification costs
class_validation_unequal=ifelse(pred_bagging_validation[,1]>=pred_bagging_validation[,2]*1.5,0,1)
tab_bagging_cost_unequal_validation = table(validation_balanced$DELAYED, class_validation_unequal)
bagging_cost_unequal_performance_validation = performance(tab_bagging_cost_unequal_validation)
bagging_cost_unequal_performance_validation

#### UNEQUAL COSTS: GRID SEARCH FOR NTREE & NODESIZE
gc()
ntree<-seq(20,40,10)
nodesize<-c(20,50,100,150,200,250,300,350,400)

df<-data.frame('nodesize'=0,'ntree'=0)

for (i in nodesize){
  for(j in ntree){
    
      temp.df<-data.frame('nodesize'=i,'ntree'=j)
      df<-rbind(df,temp.df)
    
  }
}
df<-df[-1,]
row.names(df)<-NULL
df

grid_search_bag<-data.frame('hitrate'=0,'sensitivity'=0,'specificity'=0)

for (i in 1:nrow(df)){
  
  print(i)
  
  bagging_tree=randomForest(as.factor(train_balanced$DELAYED)~.,data=train_balanced, mtry=n,nodesize=df[i,1] ,ntree=df[i,2])
  pred_bagging_validation=predict(bagging_tree,newdata=validation_balanced,type="prob")
  
  #unequal classification costs
  class_validation_unequal=ifelse(pred_bagging_validation[,1]>=pred_bagging_validation[,2]*1.5,0,1)
  tab_bagging_cost_unequal_validation = table(validation_balanced$DELAYED, class_validation_unequal)
  bagging_cost_unequal_performance_validation = performance(tab_bagging_cost_unequal_validation)
  bagging_cost_unequal_performance_validation
  
  temp<-bagging_cost_unequal_performance_validation
  grid_search_bag<-rbind(grid_search_bag,temp)
  
}
grid_search_bag<-grid_search_bag[-1,]
row.names(grid_search_bag)<-NULL
grid_search_bag


################

# Random Forest
random_forest_tree=randomForest(as.factor(train_balanced$DELAYED)~.,data=train_balanced, mtry=4, ntree=50,importance=TRUE)
random_forest_tree
round(importance(random_forest_tree),3)
varImpPlot(random_forest_tree, sort=TRUE, n.var=n)

#predictions training data
pred_rf_train=predict(random_forest_tree,newdata=train_balanced,type="prob")

#Bayes classifier
class_train_equal=ifelse(pred_rf_train[,1]>0.5,0,1)
tab_rf_cost_equal_train = table(train_balanced$DELAYED,class_train_equal)
rf_cost_equal_performance_train = performance(tab_rf_cost_equal_train)
rf_cost_equal_performance_train

#unequal classification costs
class_train_unequal=ifelse(pred_rf_train[,1]>=pred_rf_train[,2]*1.5,0,1)
tab_rf_cost_unequal_train = table(train_balanced$DELAYED,class_train_unequal)
rf_cost_unequal_performance_train = performance(tab_rf_cost_unequal_train)
rf_cost_unequal_performance_train

#predictions validation data
head(validation_balanced)
dim(validation_balanced)
table(validation_balanced$DELAYED)

pred_rf_validation=predict(random_forest_tree,newdata=validation_balanced,type="prob")

#Bayes classifier
class_validation_equal=ifelse(pred_rf_validation[,1]>0.5,0,1)
tab_rf_cost_equal_validation = table(validation_balanced$DELAYED,class_validation_equal)
rf_cost_equal_performance_validation = performance(tab_rf_cost_equal_validation)
rf_cost_equal_performance_validation

#unequal classification costs
class_validation_unequal=ifelse(pred_rf_validation[,1]>=pred_rf_validation[,2]*1.5,0,1)
tab_rf_cost_unequal_validation = table(validation_balanced$DELAYED, class_validation_unequal)
rf_cost_unequal_performance_validation = performance(tab_rf_cost_unequal_validation)
rf_cost_unequal_performance_validation

####UNEQUAL COSTS: GRID SEARCH FOR NTREE & MTRY ####
gc()
ntree<-seq(20,40,10)
mtry<-seq(2,6,2)
nodesize<-c(20,50,100,150,200,250,300,350,400)

df<-data.frame('nodesize'=0,'ntree'=0,'mtry'=0)

for (i in nodesize){
  for(j in ntree){
    for(u in mtry){
      temp.df<-data.frame('nodesize'=i,'ntree'=j,'mtry'=u)
      df<-rbind(df,temp.df)
    }
  }
}
df<-df[-1,]
row.names(df)<-NULL
df

grid_search_rf<-data.frame('hitrate'=0,'sensitivity'=0,'specificity'=0)

for (i in 1:nrow(df)){
  
  print(i)
  
  random_forest_tree=randomForest(as.factor(train_balanced$DELAYED)~.,data=train_balanced, mtry=df[i,3], ntree=df[i,2],nodesize=df[i,1])
  pred_rf_validation=predict(random_forest_tree,newdata=validation_balanced,type="prob")
  
  #unequal classification costs
  class_validation_unequal=ifelse(pred_rf_validation[,1]>=pred_rf_validation[,2]*1.5,0,1)
  tab_rf_cost_unequal_validation = table(validation_balanced$DELAYED, class_validation_unequal)
  rf_cost_unequal_performance_validation = performance(tab_rf_cost_unequal_validation)
  rf_cost_unequal_performance_validation
  
  temp<-rf_cost_unequal_performance_validation
  grid_search_rf<-rbind(grid_search_rf,temp)
  
}

grid_search_rf<-grid_search_rf[-1,]
row.names(grid_search_rf)<-NULL
grid_search_rf

bag_param<-data.frame('ntree'=20,'nodesize'=150)


n<-dim(train_balanced)[2]-1

### Fit the models that we will use for prediction

## Bagging
bag<-randomForest(as.factor(train_balanced$DELAYED)~.,data=train_balanced, mtry=n,ntree=bag_param[1,1],nodesize=bag_param[1,2],importance=TRUE)
bag


#predictions training data
pred_bag_train=predict(bag,newdata=train_balanced,type="prob")

#Bayes classifier
class_train_equal=ifelse(pred_bag_train[,1]>0.5,0,1)
tab_bag_cost_equal_train = table(train_balanced$DELAYED,class_train_equal)
bag_cost_equal_performance_train = performance(tab_bag_cost_equal_train)
bag_cost_equal_performance_train

#unequal classification costs
class_train_unequal=ifelse(pred_bag_train[,1]>=pred_bag_train[,2]*1.5,0,1)
tab_bag_cost_unequal_train = table(train_balanced$DELAYED,class_train_unequal)
bag_cost_unequal_performance_train = performance(tab_bag_cost_unequal_train)
bag_cost_unequal_performance_train

#predictions validation data
head(validation_balanced)
dim(validation_balanced)
table(validation_balanced$DELAYED)

pred_bag_validation=predict(bag,newdata=validation_balanced,type="prob")

#Bayes classifier
class_validation_equal=ifelse(pred_bag_validation[,1]>0.5,0,1)
tab_bag_cost_equal_validation = table(validation_balanced$DELAYED,class_validation_equal)
bag_cost_equal_performance_validation = performance(tab_bag_cost_equal_validation)
bag_cost_equal_performance_validation

#unequal classification costs
class_validation_unequal=ifelse(pred_bag_validation[,1]>=pred_bag_validation[,2]*1.5,0,1)
tab_bag_cost_unequal_validation = table(validation_balanced$DELAYED, class_validation_unequal)
bag_cost_unequal_performance_validation = performance(tab_bag_cost_unequal_validation)
bag_cost_unequal_performance_validation

rf_param<-data.frame('ntree'=30,'mtry'=4,'nodesize'=300)
## Random Forests
rf<-randomForest(as.factor(train_balanced$DELAYED)~.,data=train_balanced, mtry=rf_param[1,2],ntree=rf_param[1,1],nodesize=rf_param[1,3],importance=TRUE)
rf

#predictions training data
pred_rf_train=predict(rf,newdata=train_balanced,type="prob")

#Bayes classifier
class_train_equal=ifelse(pred_rf_train[,1]>0.5,0,1)
tab_rf_cost_equal_train = table(train_balanced$DELAYED,class_train_equal)
rf_cost_equal_performance_train = performance(tab_rf_cost_equal_train)
rf_cost_equal_performance_train

#unequal classification costs
class_train_unequal=ifelse(pred_rf_train[,1]>=pred_rf_train[,2]*1.5,0,1)
tab_rf_cost_unequal_train = table(train_balanced$DELAYED,class_train_unequal)
rf_cost_unequal_performance_train = performance(tab_rf_cost_unequal_train)
rf_cost_unequal_performance_train

#predictions validation data
head(validation_balanced)
dim(validation_balanced)
table(validation_balanced$DELAYED)

pred_rf_validation=predict(rf,newdata=validation_balanced,type="prob")

#Bayes classifier
class_validation_equal=ifelse(pred_rf_validation[,1]>0.5,0,1)
tab_rf_cost_equal_validation = table(validation_balanced$DELAYED,class_validation_equal)
rf_cost_equal_performance_validation = performance(tab_rf_cost_equal_validation)
rf_cost_equal_performance_validation

#unequal classification costs
class_validation_unequal=ifelse(pred_rf_validation[,1]>=pred_rf_validation[,2]*1.5,0,1)
tab_rf_cost_unequal_validation = table(validation_balanced$DELAYED, class_validation_unequal)
rf_cost_unequal_performance_validation = performance(tab_rf_cost_unequal_validation)
rf_cost_unequal_performance_validation

##### TEST SET PREDICTIONS #####

## Predictions

bag_pred <- predict(bag,newdata = test_set_final, type = 'prob')

#unequal classification costs
class_test_unequal_bag=ifelse(bag_pred[,1]>=bag_pred[,2]*1.5,0,1)
tab_bag_cost_unequal_test = table(test_set_final$DELAYED, class_test_unequal_bag)
tab_bag_cost_unequal_test   #### AMOUNT OF DELAYED FLIGHTS THAT IS GOING TO BE SEEN IN THE TEST SET - BAGGING ENSEMBLE

rf_pred <- predict(rf,newdata = test_set_final, type = 'prob')

#unequal classification costs
class_test_unequal_rf=ifelse(rf_pred[,1]>=rf_pred[,2]*1.5,0,1)
tab_rf_cost_unequal_test = table(test_set_final$DELAYED, class_test_unequal_rf)
tab_rf_cost_unequal_test   #### AMOUNT OF DELAYED FLIGHTS THAT IS GOING TO BE SEEN IN THE TEST SET - RF MODEL

a = data.frame(class_test_unequal_bag)
head(a)

b = data.frame(class_test_unequal_rf)
head(b)

test_set_final["BAG_PRED"] = a$class_test_unequal_bag
test_set_final["RF_PRED"] = b$class_test_unequal_rf

head(test_set_final)

### BAGGING ###

# number of non-delayed flights per month day
b1 = table(test_set_final$MONTHDAY_BIN[test_set_final$BAG_PRED==0])
# number of delayed flights per month day
b2 = table(test_set_final$MONTHDAY_BIN[test_set_final$BAG_PRED==1])

round(b2/(b1+b2)*100)

# number of non-delayed flights per week day
b3 = table(test_set_final$WEEKDAY_BIN[test_set_final$BAG_PRED==0])
# number of delayed flights per week day
b4 = table(test_set_final$WEEKDAY_BIN[test_set_final$BAG_PRED==1])

round(b4/(b3+b4)*100)

# number of non-delayed flights per hour
b5=table(test_set_final$HOUR_BIN[test_set_final$BAG_PRED==0])
# number of delayed flights per hour
b6=table(test_set_final$HOUR_BIN[test_set_final$BAG_PRED==1])

round(b6/(b6+b5)*100)

# number of non-delayed flights per airline
b7=table(test_set_final$AIRLINE[test_set_final$BAG_PRED==0])
# number of delayed flights per airline
b8=table(test_set_final$AIRLINE[test_set_final$BAG_PRED==1])

round(b8/(b8+b7)*100)
airlines

### RF ###

# number of non-delayed flights per month day
rf1=table(test_set_final$MONTHDAY_BIN[test_set_final$RF_PRED==0])
# number of delayed flights per month day
rf2=table(test_set_final$MONTHDAY_BIN[test_set_final$RF_PRED==1])

round(rf2/(rf2+rf1)*100)

# number of non-delayed flights per week day
rf3=table(test_set_final$WEEKDAY_BIN[test_set_final$RF_PRED==0])
# number of delayed flights per week day
rf4=table(test_set_final$WEEKDAY_BIN[test_set_final$RF_PRED==1])

round(rf4/(rf4+rf3)*100)

# number of non-delayed flights per hour
rf5=table(test_set_final$HOUR_BIN[test_set_final$RF_PRED==0])
# number of delayed flights per hour
rf6=table(test_set_final$HOUR_BIN[test_set_final$RF_PRED==1])

round(rf6/(rf6+rf5)*100)

# number of non-delayed flights per airline
rf7=table(test_set_final$AIRLINE[test_set_final$RF_PRED==0])
# number of delayed flights per airline
rf8=table(test_set_final$AIRLINE[test_set_final$RF_PRED==1])
airlines


barplot(b2/(b1+b2)*100, xlab = "Month day bins", ylab = "Percentage of delayed flights", ylim=c(0,100))
barplot(b4/(b3+b4)*100, xlab = "Week day bins", ylab = "Percentage of delayed flights", ylim=c(0,100))
barplot(b6/(b5+b6)*100, xlab = "Hour bins", ylab = "Percentage of delayed flights", ylim=c(0,100))

bair = b8/(b7+b8)*100
bair1 = data.frame(bair)
barplot(bair1$Freq~bair1$Var1, xlab = "Airlines", ylab = "Percentage of delayed flights", ylim=c(0,100))

barplot(rf2/(rf1+rf2)*100, xlab = "Month day bins", ylab = "Percentage of delayed flights", ylim=c(0,100))
barplot(rf4/(rf3+rf4)*100, xlab = "Week day bins", ylab = "Percentage of delayed flights", ylim=c(0,100))
barplot(rf6/(rf5+rf6)*100, xlab = "Hour bins", ylab = "Percentage of delayed flights", ylim=c(0,100))

rf8[2]
rf8df = data.frame(c("AA", "AS","B6","DL","EV","F9","HA","MQ","NK","OO","UA","US","VX","WN"),
                   c(6421, 0,  1778,  1570,  9997,  3380,   544,  6659,  7127,  4580, 10906,  1534,  1238, 30588))
names(rf8df) <- c("Var1", "Freq")

rf7df = data.frame(rf7)
rfair = rf8df$Freq/(rf7df$Freq+rf8df$Freq)*100
rfair1 = data.frame(rf8df$Var1,rfair)
barplot(rfair1$rfair~rfair1$rf8df.Var1, xlab = "Airlines", ylab = "Percentage of delayed flights", ylim=c(0,100))

