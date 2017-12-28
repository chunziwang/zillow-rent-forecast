library(fpp2)
library(readr)
library(plyr)
library(dplyr)
library(quantmod)
library(ggplot2)

# Import dataset, name it monthly_zillow_census 
monthly_zillow_census <- read_csv("~/monthly_zillow_census.csv")

# Remove unwanted column X1
monthly_zillow_census <- monthly_zillow_census[,c(2:10)]

# Add prior year's rent column
monthly = monthly_zillow_census %>%
  group_by(Metro,County,State)%>%
  mutate(lag_rent = lag(Rent, n= 12, order_by=Date))%>%
  ungroup()

# Calculate Mean Monthly Rent by Metro
monthly_rent_metro = aggregate(monthly$Rent ~ monthly$Metro + 
                                 monthly$State + monthly$Year + monthly$Date + 
                                 monthly$Month, monthly, mean)

# Calculate Mean Monthly Income by Metro
monthly_income_metro = aggregate(as.numeric(monthly$Med_Monthly_Income) ~ monthly$Metro + 
                                   monthly$State + monthly$Year + monthly$Date + 
                                   monthly$Month, monthly, mean)

# Calculate Population by Metro
monthly_population_metro = aggregate(as.numeric(monthly$County_Population) ~ monthly$Metro + 
                                       monthly$State + monthly$Year + monthly$Date + 
                                       monthly$Month, monthly, sum)

# Create a new metro dataframe of the above three combined
zillow_metro_monthly <- cbind(monthly_rent_metro,monthly_income_metro$'as.numeric(monthly$Med_Monthly_Income)',monthly_population_metro$'as.numeric(monthly$County_Population)')

# Rename Columns 
zillow_metro_monthly = rename(zillow_metro_monthly, 
                              Metro = "monthly$Metro",
                              State = "monthly$State", 
                              Year = "monthly$Year",
                              Date = "monthly$Date", 
                              Month = "monthly$Month",
                              Med_Monthly_Income = "monthly_income_metro$\"as.numeric(monthly$Med_Monthly_Income)\"",
                              Avg_Monthly_Rent = "monthly$Rent",
                              Population = "monthly_population_metro$\"as.numeric(monthly$County_Population)\"")

#View the zillow_metro_monthly dataframe with population in descending ranking
metro_rank <- zillow_metro_monthly %>% 
  arrange(desc(Population))

#Find out New York is classified into two parts:New York,NY and New York,NJ

#combine ny,ny and ny,nj
ny_ny <- metro_rank[c(97:144),]
ny_nj <- metro_rank[c(193:240),]
ny <- merge(ny_ny,ny_nj,by=c("Date"))
ny$Avg_Monthly_Rent <- (ny$Avg_Monthly_Rent.x+ny$Avg_Monthly_Rent.y)/2
ny$Med_Monthly_Income <- (ny$Med_Monthly_Income.x+ny$Med_Monthly_Income.y)/2
ny$Population <- ny$Population.x+ny$Population.y
ny <- ny[,-c(6:15)]
ny = rename(ny,
            Metro="Metro.x",
            State="State.x",
            Year = "Year.x",
            Month = "Month.x")

#combine ny into metro_rank
metro_rank <- rbind(metro_rank,ny)
metro_rank <- metro_rank[-c(97:144,193:240),]
metro_rank <- metro_rank %>%
  arrange(desc(Population))

#select top20 metro
metro_top20 <- metro_rank[c(1:948,1021:1032),]

#Time Series Forecasting
#New York, NY Metro area
ny <- metro_top20[metro_top20$Metro == "New York, New York",]
ny <- ny %>% arrange(Date)

#transform data.frame to time.series object
ny_ts <- ts(ny[,6],start=c(2013,1),end=c(2016,12),frequency = 12)

#plot the time series
autoplot(ny_ts) + 
  xlab("Year") + 
  ylab("Monthly Rent") +
  ggtitle("New York Metro Area")

#check seasonality,which is not too obvious
ggseasonplot(ny_ts)

#separate train set and test set
train <- window(ny_ts,end=c(2015,12))
test <- window(ny_ts,start=c(2016,1))

#test various models on train set to find the most accurate one
#test arima model
#fit arima model to training data
fit_arima <- auto.arima(train) #ARIMA(3,1,0) with drift 
#check residuals
checkresiduals(fit_arima) #white noise <- TRUE p-value greater than 0.05 indicates that the data resembles white noise
#produce forecast
arima_fc <- forecast(fit_arima,h=12)
#plot
autoplot(arima_fc) +
  autolayer(test,series="Test data")

accuracy(arima_fc,test) #Test set RMSE = 24.28
checkresiduals(arima_fc) #white noise <- TRUE p-value greater than 0.05 indicates that the data resembles white noise

#test naive model
naive_fc <- naive(train,h=12)
autoplot(naive_fc) +
  autolayer(test,series="Test data")

accuracy(naive_fc,test) #RMSE = 38.40
checkresiduals(naive_fc) #white noise <- FALSE p-value less than 0.05

#test snaive model
snaive_fc <- snaive(train,h=12)
autoplot(snaive_fc) +
  autolayer(test,series="Test data")

accuracy(snaive_fc,test) #RMSE = 103.66
checkresiduals(snaive_fc) #white noise <- FALSE p-value less than 0.05

#test ses model
ses_fc <- ses(train,h=12)
autoplot(ses_fc) +
  autolayer(test,series="Test data")

accuracy(ses_fc,test) #RMSE = 38.41
checkresiduals(ses_fc) #white noise <- FALSE p-value less than 0.05

#test holt's linear trend model
holt_fc <- holt(train,h=12)
autoplot(holt_fc) +
  autolayer(test,series="Test data")

accuracy(holt_fc,test) #RMSE = 23.49
checkresiduals(holt_fc) #white noise <- FALSE p-value less than 0.05

#test holt's damped trend model
holt_damped_fc <- holt(train,damped=TRUE,h=12)
autoplot(holt_damped_fc) +
  autolayer(test,series="Test data")

accuracy(holt_damped_fc,test) #RMSE = 12.51
checkresiduals(holt_damped_fc) #white noise <- FALSE p-value less than 0.05

#test hw model
hw_fc <- hw(train,h=12)
autoplot(hw_fc) +
  autolayer(test,series="Test data")

accuracy(hw_fc,test) #RMSE = 31.39
checkresiduals(hw_fc) #white noise <- FALSE p-value less than 0.05

#test ets model
fit_ets <- ets(train) #ETS(M,A,N)
ets_fc <- forecast(fit_ets,h=12)
autoplot(ets_fc) +
  autolayer(test,series="Test data")

accuracy(ets_fc,test) #RMSE = 23.5
checkresiduals(fit_ets) #white noise <- FALSE p-value less than 0.05

## Based on choosing model with lowest RMSE and white noise residuals choose arima model. 

#Time Series Forecasting With Dynamic Regression to factor in income and population
#New York, NY Metro area
#transform data.frame to time.series object
ny_ts_dr <- ts(ny[,6:8],start=c(2013,1),end=c(2016,12),frequency = 12)

#plot the time series
autoplot(ny_ts_dr,facets=TRUE) + 
  xlab("Year") + 
  ylab("") +
  ggtitle("Monthly Rent, Med Income and Population")

#check correlation of income and rent 
ggplot(as.data.frame(ny_ts_dr),
       aes(x=Med_Monthly_Income,y=Avg_Monthly_Rent)) +
  geom_point() +
  ggtitle("Monthly Rent vs. Median Household Income in New York")

#check correlation of population and rent 
ggplot(as.data.frame(ny_ts_dr),
       aes(x=Population,y=Avg_Monthly_Rent)) +
  geom_point() +
  ggtitle("Monthly Rent vs. Population New York")

#separate train set and test set
train_dr <- window(ny_ts_dr,end=c(2015,12))
test_dr <- window(ny_ts_dr,start=c(2016,1))

#set up another test set including only rent for accuracy calculation
test_dr2 <- ts(ny[,6],start=c(2013,1),end=c(2016,12),frequency = 12)
test_dr2 <- window(test_dr2,start=c(2016,1))

#fit dynamic regression model to training data
fit <- auto.arima(train_dr[,"Avg_Monthly_Rent"],
                  xreg = train_dr[,"Med_Monthly_Income"])

#Regression with ARIMA(0,0,1) errors
#xreg=0.8514,which means 1% of income change results in 0.8514% rent change

fit2 <- auto.arima(train_dr[,"Avg_Monthly_Rent"],
                   xreg = train_dr[,"Population"])

#xreg=0,which means 1% of income change results in 0% rent change, use income instead

#test
#produce forecast of 2016
fc_income <- forecast(fit,xreg=rep(6993.611,12))
autoplot(fc_income) +
  autolayer(test,series="Test data")

#check accuracy
accuracy(fc_income, test_dr2) #test set RMSE = 119.63
## RMSE higher for dynamic regression than arima
#check residuals
checkresiduals(fc_income) #white noise <- FALSE p-value less than 0.05

#compare 2016 forecast with actuals 
#convert test_df to dataframe class
test_dr_df <- as.data.frame(test_dr)

#remove unwanted columns
test_dr_df$Med_Monthly_Income <- NULL
test_dr_df$Population <- NULL

#add predecition to test_dr_df to compute accuracy
test_dr_df$predict_rent <- as.numeric(fc_income$mean)
#check accuracy
test_dr_df$accuracy_rent <- abs((test_dr_df$Avg_Monthly_Rent-test_dr_df$predict_rent)/test_dr_df$Avg_Monthly_Rent)*100
#Find MAPE
mean(test_dr_df$accuracy_rent) #4.740585

###
#Compare to auto.arima model
#compare 2016 forecast with actuals 
#convert test to dataframe class
test_df <- as.data.frame(test)

#Rename column
colnames(test_df) <- "Avg_Monthly_Rent"

#add predecition to test_df to compute accuracy
test_df$predict_rent <- as.numeric(arima_fc$mean)
#check accuracy
test_df$accuracy_rent <- abs((test_df$Avg_Monthly_Rent-test_df$predict_rent)/test_df$Avg_Monthly_Rent)*100
#Find MAPE
mean(test_df$accuracy_rent) #0.6822351

#Confirms RMSE findings
#Based on the principle of choosing the model
#with best accuracy on test set, we reject Dynamic Regression model

#Use Arima Model to Test for top 20 metros
# Make a list of Metro Top20
metros <- list("Atlanta, Georgia","Boston, Massachusetts","Chicago, Illinois",
               "Dallas-Fort Worth, Texas","Denver, Colorado","Detroit, Michigan",
               "Houston, Texas","Los Angeles, California","Miami-Fort Lauderdale, Florida",
               "Minneapolis-St Paul, Minnesota","New York, New York","Orlando, Florida",
               "Philadelphia, Pennsylvania","Phoenix, Arizona","Riverside, California",
               "San Diego, California","San Francisco, California","Seattle, Washington",
               "Tampa, Florida","Washington, Virginia")

train_fc <- function(metro) {
  metro_df <- metro_top20[which(metro_top20$Metro==metro),]
  metro_df <- metro_df %>% arrange(Date)
  metro_ts <- ts(metro_df[,6],start=c(2013,1),end=c(2016,12),frequency = 12)
  metro_train <- window(metro_ts,end=c(2015,12))
  fit <- auto.arima(metro_train)
  fc <- forecast(fit,h=12)
  fc_rent <- fc$mean
  return(fc_rent)
}

test_2016 <- function(metro) {
  metro_df <- metro_top20[which(metro_top20$Metro==metro),]
  metro_df <- metro_df %>% arrange(Date)
  metro_ts <- ts(metro_df[,6],start=c(2013,1),end=c(2016,12),frequency = 12)
  metro_test <- window(metro_ts,start=c(2016,1))
  test_df <- as.data.frame(metro_test)
  colnames(test_df) <- "actual_rent"
  return(test_df)
}

metro_2016 <- function(metro) {
  metro_rent_2016 <- cbind(test_2016(metro),as.data.frame(train_fc(metro)))
  colnames(metro_rent_2016)[2] <- "predict_rent"
  metro_rent_2016$accuracy_rent <- abs((metro_rent_2016$actual_rent-metro_rent_2016$predict_rent)/metro_rent_2016$actual_rent)*100
  metro_rent_2016$dif_rent <- (metro_rent_2016$predict_rent-metro_rent_2016$actual_rent)^2
  return(metro_rent_2016)
}

# Find accuracy of arima model on every top20 metro areas
atlanta_2016 <- metro_2016(metros[1])
#Find MAPE
mean(atlanta_2016$accuracy_rent) #0.2365222

boston_2016 <- metro_2016(metros[2])
mean(boston_2016$accuracy_rent) #0.4200103

chicago_2016 <- metro_2016(metros[3])
mean(chicago_2016$accuracy_rent) #0.9978045

dallas_2016 <- metro_2016(metros[4])
mean(dallas_2016$accuracy_rent) #0.2035519

denver_2016 <- metro_2016(metros[5])
mean(denver_2016$accuracy_rent) #1.850447

detroit_2016 <- metro_2016(metros[6])
mean(detroit_2016$accuracy_rent) #1.619301

houston_2016 <- metro_2016(metros[7])
mean(houston_2016$accuracy_rent) #3.38999

la_2016 <- metro_2016(metros[8])
mean(la_2016$accuracy_rent) #0.9903569

miami_2016 <- metro_2016(metros[9])
mean(miami_2016$accuracy_rent) #1.15319

minneapolis_2016 <- metro_2016(metros[10])
mean(minneapolis_2016$accuracy_rent) #2.452473

ny_2016 <- metro_2016(metros[11])
mean(ny_2016$accuracy_rent) #0.6822351

orlando_2016 <- metro_2016(metros[12])
mean(orlando_2016$accuracy_rent) #0.4094452

philadelphia_2016 <- metro_2016(metros[13])
mean(philadelphia_2016$accuracy_rent) #0.4034772

phoenix_2016 <- metro_2016(metros[14])
mean(phoenix_2016$accuracy_rent) #2.437522

riverside_2016 <- metro_2016(metros[15])
mean(riverside_2016$accuracy_rent) #2.219489

sandiego_2016 <- metro_2016(metros[16])
mean(sandiego_2016$accuracy_rent) #0.4120787

sf_2016 <- metro_2016(metros[17])
mean(sf_2016$accuracy_rent) #3.118529

seattle_2016 <- metro_2016(metros[18])
mean(seattle_2016$accuracy_rent) #0.5098476

tampa_2016 <- metro_2016(metros[19])
mean(tampa_2016$accuracy_rent) #0.5662547

washington_2016 <- metro_2016(metros[20])
mean(washington_2016$accuracy_rent) #0.4706343

#Find average MAPE for top20 metros
sum=0
for(i in 1:length(metros)) {
  sum=mean(metro_2016(metros[i])$accuracy_rent)+sum
}
avg_accuracy <- sum/20
#1.227158

#Find RMSE for top20 metros
sum=0
for(i in 1:length(metros)) {
  sum=mean(metro_2016(metros[i])$dif_rent)+sum
}
rmse_2016 <- (sum/20)^.5 #50.69577

###Decision Tree 
# Load Libraries
library(rpart)
library(rpart.plot)

#Convert Metro and State to Factors
metro_top20$Metro <- as.factor(metro_top20$Metro)
metro_top20$State <- as.factor(metro_top20$State)

# Add prior year's rent column
metro_top20_with_lag = metro_top20 %>%
  group_by(Metro,State)%>%
  mutate(lag_rent = lag(Avg_Monthly_Rent, n= 12, order_by=Date))%>%
  ungroup()

# Add prior year's income column
metro_top20_with_lag = metro_top20_with_lag %>%
  group_by(Metro,State)%>%
  mutate(lag_income = lag(Med_Monthly_Income, n= 12, order_by=Date))%>%
  ungroup()

# Add prior year's population column
metro_top20_with_lag = metro_top20_with_lag %>%
  group_by(Metro,State)%>%
  mutate(lag_population = lag(Population, n= 12, order_by=Date))%>%
  ungroup()

# Add % change in rent column
#Arrange metro_top20_with_lag dataframe by date
metro_top20_with_lag <- metro_top20_with_lag %>% 
  arrange(Metro, Date)

# Calculate year over year change in monthly rent
metro_top20_with_lag$Percent_MonthlyRent_Change <- Delt(as.numeric(metro_top20_with_lag$Avg_Monthly_Rent), k = 12)
metro_top20_with_lag$Percent_MonthlyRent_Change[which(metro_top20_with_lag$Year=="2013"),] <- NA
metro_top20_with_lag$Percent_MonthlyRent_Change = as.numeric(metro_top20_with_lag$Percent_MonthlyRent_Change)

#Remove 2013 data as it has NA's
metro_top20_with_lag <- na.omit(metro_top20_with_lag)

# Create 2017 dataframe for tree1
metro_top20_2017 = metro_top20_with_lag[,c("Metro","State","Year","Date", "Month", "Avg_Monthly_Rent", "Med_Monthly_Income", "Population")]

#Keep only 2016 data
metro_top20_2017 = metro_top20_2017[metro_top20_2017$Year == 2016, ]

#Rename Columns
metro_top20_2017 = rename(metro_top20_2017,
                          lag_rent = "Avg_Monthly_Rent",
                          lag_income = "Med_Monthly_Income",
                          lag_population = "Population")
#Change Year to 2017
metro_top20_2017$Year[metro_top20_2017$Year == "2016"] <- "2017"
metro_top20_2017$Date = metro_top20_2017$Date +366

#Change Year column to numeric
metro_top20_2017$Year = as.numeric(metro_top20_2017$Year)

#Create 2017 Dataframe for tree2
metro_top20_2017_Change = metro_top20_2017

#Create 2017 Dataframe for RF1
metro_top20_2017_RF1 = metro_top20_2017

#Create 2017 Dataframe for RF2
metro_top20_2017_RF2 = metro_top20_2017

# Split the Data
trainDT = metro_top20_with_lag[metro_top20_with_lag$Year %in% c(2014:2015),]
testDT = metro_top20_with_lag[metro_top20_with_lag$Year %in% c(2016),]

#Monthly Rent as Target Variable
#Find correlation with Rent
cor(metro_top20_with_lag[,c(3, 5:12)],use='complete.obs')[,3]
#lag rent = .99, Median Monthly Income = .60, lag income = .57,  
#Rent Change = .52 Population = .27, lag population = .27 

# Regression Tree with Rent as Target Variable
tree1 = rpart(Avg_Monthly_Rent ~ Metro + Year + Date + State +
                Month + lag_rent + lag_income + lag_population, data=trainDT,method='anova',cp=0.001) 

rpart.plot(tree1)

printcp(tree1) # display the results,  
plotcp(tree1) # visualize cross-validation results, 
summary(tree1) # detailed summary of splits

#Prune back the tree to avoid overfitting the data. 
#Typically, you will want to select a tree size that minimizes the cross-validated error, the xerror column printed by printcp( )
#Find best CP
tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]

#Prune tree
tree1_pruned = prune(tree1, cp=0.001) #same cp used above, tree not pruned
rpart.plot(tree1_pruned)

# Predict and evaluate on test set
test.pred.rtree <- predict(tree1_pruned,testDT) 
RMSE.rtree <- sqrt(mean((test.pred.rtree-testDT$Avg_Monthly_Rent)^2))
RMSE.rtree #RMSE = 106.8108

MAE.rtree <- mean(abs(test.pred.rtree-testDT$Avg_Monthly_Rent))
MAE.rtree #MAE = 78.95

SSE.rtree = sum((test.pred.rtree-testDT$Avg_Monthly_Rent)^2)
SSE.rtree #SSE = 2738052

MAPE.rtree = mean(abs((testDT$Avg_Monthly_Rent-test.pred.rtree)/testDT$Avg_Monthly_Rent)*100)
MAPE.rtree #MAPE = 3.492399

### Decision Tree with Rent % Change as Target Variable
#Find correlation with Rent Change
cor(metro_top20_with_lag[,c(3, 5:12)],use='complete.obs')[,9]
#Avg Monthly Rent = .51, lag rent = .43, Median Monthly Income = .09 
#lag income = .06, Population = .03 lag population = .02

# Regression Tree with Rent Change as Target Variable
tree2 = rpart(Percent_MonthlyRent_Change~Metro + Year + Date + State +
                Month + lag_population + lag_rent + lag_income, data=trainDT,method='anova',cp=0.001) 

rpart.plot(tree2)

printcp(tree2) # display the results,  
plotcp(tree2) # visualize cross-validation results, 
summary(tree2) # detailed summary of splits, I like that this shows the variable importance

#Prune back the tree to avoid overfitting the data. 
#Typically, you will want to select a tree size that minimizes the cross-validated error, the xerror column printed by printcp( )
#Find best CP
tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"]

#Prune tree
tree2_pruned = prune(tree2, cp=.001) #same cp used above, tree not pruned
rpart.plot(tree2_pruned)

# Predict and evaluate on test set
test.pred.rtree2 <- predict(tree2_pruned,testDT) 
RMSE.rtree2 <- sqrt(mean((test.pred.rtree2-testDT$Percent_MonthlyRent_Change)^2))
RMSE.rtree2 #RMSE = 0.03080689

MAE.rtree2 <- mean(abs(test.pred.rtree2-testDT$Percent_MonthlyRent_Change))
MAE.rtree2 #MAE = 0.02014408

SSE.rtree2 = sum((test.pred.rtree2-testDT$Percent_MonthlyRent_Change)^2)
SSE.rtree2 #SSE = 0.2277755

MAPE.rtree2 = mean(abs((testDT$Percent_MonthlyRent_Change-test.pred.rtree2)/testDT$Percent_MonthlyRent_Change)*100)
MAPE.rtree2 #MAPE = 158.2011

# Random Forest 
# Load library
library(randomForest)

# Random Forest with Rent as Target Variable
Rent.rf=randomForest(Avg_Monthly_Rent ~ Metro + Year + Date + State +
                       Month + lag_rent + lag_income + lag_population, data = trainDT)
Rent.rf

# See what variables are important (have the highest impact on rf model)
varImpPlot(Rent.rf)

# Predict and evaluate on test set 
# These values change slightly each time you run it
test.pred.rf <- predict(Rent.rf,testDT) 
RMSE.rf <- sqrt(mean((test.pred.rf-testDT$Avg_Monthly_Rent)^2))
RMSE.rf #RMSE = 79.34507

MAE.rf <- mean(abs(test.pred.rf-testDT$Avg_Monthly_Rent))
MAE.rf #MAE = 55.85307

SSE.rf = sum((test.pred.rf-testDT$Avg_Monthly_Rent)^2)
SSE.rf #SSE = 1510954

MAPE.rf = mean(abs((testDT$Avg_Monthly_Rent-test.pred.rf)/testDT$Avg_Monthly_Rent)*100)
MAPE.rf #MAPE = 2.435211

#### RMSE of RF is less than RMSE for DT, reject DT (The root mean squared error can only be compared between models whose errors are measured in the same units, which is true in this case).

# Random Forest with Rent % Change as Target Variable
Change.rf=randomForest(Percent_MonthlyRent_Change~Metro + Year + Date + State +
                         Month + lag_population + lag_rent + lag_income, data = trainDT)
Change.rf

# See what variables are important (have the highest impact on rf model)
varImpPlot(Change.rf)

# Predict and evaluate on test set
# These values change slightly each time you run it
test.pred.rf2 <- predict(Change.rf,testDT) 
RMSE.rf2 <- sqrt(mean((test.pred.rf2-testDT$Percent_MonthlyRent_Change)^2))
RMSE.rf2 #RMSE = 0.02717147

MAE.rf2 <- mean(abs(test.pred.rf2-testDT$Percent_MonthlyRent_Change))
MAE.rf2 #MAE = 0.01820294

SSE.rf2 = sum((test.pred.rf2-testDT$Percent_MonthlyRent_Change)^2)
SSE.rf2 #SSE = 0.1771893

MAPE.rf2 = mean(abs((testDT$Percent_MonthlyRent_Change-test.pred.rf2)/testDT$Percent_MonthlyRent_Change)*100)
MAPE.rf2 #MAPE = 135.5084

#### RMSE of RF is less than RMSE for DT, reject DT
#### MAPE - scale independent used to compare models with target variables in different units 
#### MAPE for % change is greater than MAPE for Rent as target variable, reject % change RF
#### RMSE of RF with Rent as target variable is greater than RMSE for Arima, reject RF, use Arima for forecasting

#Forecast 2017 Rent with Arima model
# Put arima modeling forecast into a function
ts_forecast <- function(metro) {
  metro_df <- metro_top20[which(metro_top20$Metro==metro),]
  metro_df <- metro_df %>% arrange(Date)
  metro_ts <- ts(metro_df[,6],start=c(2013,1),end=c(2016,12),frequency = 12)
  fit <- auto.arima(metro_ts)
  fc <- forecast(fit,h=12)
  print(autoplot(fc)+xlab("Year")+ylab("Monthly Rent"))
  summary(fc)
  checkresiduals(fc)
}

# Run forecasting on every top20 metro areas
atlanta_forecast <- ts_forecast(metros[1])
#RMSE = 0.96, AICc = 146.31, white noise <- TRUE

boston_forecast <- ts_forecast(metros[2])
#RMSE = 3.32, AICc = 262.98, white noise <- TRUE

chicago_forecast <- ts_forecast(metros[3])
#RMSE = 1.73, AICc = 195.13, white noise <- TRUE

dallas_forecast <- ts_forecast(metros[4])
#RMSE = 2.15, AICc = 212.71, white noise <- TRUE

denver_forecast <- ts_forecast(metros[5])
#RMSE = 4.82, AICc = 279, white noise <- FALSE

detroit_forecast <- ts_forecast(metros[6])
#RMSE = 1.83, AICc = 213.64, white noise <- FALSE

houston_forecast <- ts_forecast(metros[7])
#RMSE = 3.86, AICc = 258.59, white noise <- FALSE

la_forecast <- ts_forecast(metros[8])
#RMSE = 2.58, AICc = 236.25, white noise <- TRUE

miami_forecast <- ts_forecast(metros[9])
#RMSE = 2.37, AICc = 222.65, white noise <- FALSE

minneapolis_forecast <- ts_forecast(metros[10])
#RMSE = 2.26, AICc = 228.66, white noise <- TRUE

ny_forecast <- ts_forecast(metros[11])
#RMSE = 3.50, AICc = 264.62, white noise <- TRUE

orlando_forecast <- ts_forecast(metros[12])
#RMSE = 1.47, AICc = 179.79, white noise <- TRUE

philadelphia_forecast <- ts_forecast(metros[13])
#RMSE = 1.85, AICc = 204.6, white noise <- TRUE

phoenix_forecast <- ts_forecast(metros[14])
#RMSE = 1.47, AICc = 184.4, white noise <- TRUE

riverside_forecast <- ts_forecast(metros[15])
#RMSE = 1.96, AICc = 207.36, white noise <- TRUE

sandiego_forecast <- ts_forecast(metros[16])
#RMSE = 4.85, AICc = 292.77, white noise <- TRUE

sf_forecast <- ts_forecast(metros[17])
#RMSE = 7.06, AICc = 330.26, white noise <- TRUE

seattle_forecast <- ts_forecast(metros[18])
#RMSE = 2.30, AICc = 228.24, white noise <- TRUE

tampa_forecast <- ts_forecast(metros[19])
#RMSE = 1.40, AICc = 176.06, white noise <- TRUE

washington_forecast <- ts_forecast(metros[20])
#RMSE = 1.65, AICc = 193.41, white noise <- TRUE

#Create new dataframe for 2017 TS data
TSmetro_top20_2017 = metro_top20[,c("Metro","Year","Avg_Monthly_Rent")]

#Keep only 2016 data
TSmetro_top20_2017 = TSmetro_top20_2017[TSmetro_top20_2017$Year == 2016, ]

#Rename Columns
TSmetro_top20_2017 = rename(TSmetro_top20_2017,
                            Metro = "Metro",
                            Rent_2016 = "Avg_Monthly_Rent")
#Remove Year Column
TSmetro_top20_2017$Year = NULL

#Find average monthly rent for 2016
TSmetro_top20_2017_Avg = aggregate(TSmetro_top20_2017$Rent_2016 ~ TSmetro_top20_2017$Metro,
                                   TSmetro_top20_2017, mean)

#Rename Columns
TSmetro_top20_2017_Avg = rename(TSmetro_top20_2017_Avg,
                                Metro = "TSmetro_top20_2017$Metro",
                                Rent_2016 = "TSmetro_top20_2017$Rent_2016")


# put calculating mean monthly rent of 2017 into a function
ts_fc_value <- function(metro) {
  metro_df <- metro_top20[which(metro_top20$Metro==metro),]
  metro_df <- metro_df %>% arrange(Date)
  metro_ts <- ts(metro_df[,6],start=c(2013,1),end=c(2016,12),frequency = 12)
  fit <- auto.arima(metro_ts)
  fc <- forecast(fit,h=12)
  mean_2017 <- mean(fc$mean)
  print(mean_2017)
}

# Add rent_2017 and percent_change column to dataframe
for(i in 1:length(metros)) {
  TSmetro_top20_2017_Avg$Rent_2017[i] <- ts_fc_value(metros[i])
}

TSmetro_top20_2017_Avg$Percent_MonthlyRent_Change <- ((TSmetro_top20_2017_Avg$Rent_2017/TSmetro_top20_2017_Avg$Rent_2016)-1)

#Arrange with % change in descending ranking
TSmetro_top20_2017_Avg <- TSmetro_top20_2017_Avg %>% 
  arrange(desc(Percent_MonthlyRent_Change))

# Bar chart of rent change from 2016 to 2017 of top20 metro
ggplot(TSmetro_top20_2017_Avg,aes(x=reorder(Metro,-Percent_MonthlyRent_Change),y=Percent_MonthlyRent_Change)) +
  geom_bar(stat="identity",fill="orange") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  geom_text(aes(label=specify_decimal(TSmetro_top20_2017_Avg$Percent_MonthlyRent_Change,3)), vjust=1.6, color="black", size=3.5) +
  xlab("Metro") +
  ylab("Monthly Rent Change in 2017") +
  ggtitle("Top20 Metro area in US")
