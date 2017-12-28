# Load libraries
library(readr)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(censusapi)
library(quantmod)
library(lubridate)
library(ggplot2)

# Import dataset, name it price 
price = read_csv("C:\\Users\\Adimn\\Desktop\\zillow project\\price.csv")

# Examine data
str(price)
head(price, 10)

# Summarize the data
# NA's occur from Nov-10 through Mar-12
summary(price)

# Remove 2010, 2011 and 2012 as those years contained NA's. Remove Jan-17 as it is a partial year
# Remove unnescessary columns - City Code and Population Rank
price <- price[,c(2:5,33:80)]

# Reshape data from wide to long format
price = gather(data = price, "Month", "Price", 5:52, factor_key = T)

# Check if there're any outliers by summary and boxplot
summary(price$Price)
boxplot(price$Price, horizontal = TRUE)

# View the outliers in detail to see if they are unreasonble
price[which(price$Price > 15000),]
# We see cities that have the highest rent price are in Jupiter Island, Florida,
# Given its oceanside location, the high rent is reasonable and unlikely to be a data entry error 

# Split month and year into new df
list = strsplit(as.character(price$Month), "-")
df = ldply(list)
colnames(df) = c("Mon", "Year")

# Combine two dataframes 
price_new = cbind(df, price)

# Add "20" to year date
price_new$Year = paste("20",price_new$Year,sep ="")

# Change State Abbreviation to full name to match census format
price_new$State = state.name[match(price_new$State,state.abb)]

# Combine County and State Column into County to match census format
price_new$County = paste(price_new$County, "County", sep=" ")
price_new$County = paste(price_new$County, price_new$State, sep=", ")

# Combine City and State Column into City so that each City has a unique name
price_new$City = paste(price_new$City, sep=" ")
price_new$City = paste(price_new$City, price_new$State, sep=", ")

# Combine Metro and State Column into Metro so that each Metro has a unique name
price_new$Metro = paste(price_new$Metro, sep=" ")
price_new$Metro = paste(price_new$Metro, price_new$State, sep=", ")

# Calculate Annual Rent by County, and remove unwanted columns
# Sum monthly rents by City, for annual City rent
price_sum = aggregate(price_new$Price ~ price_new$City + 
                        price_new$Year + 
                        price_new$County +
                        price_new$Metro +
                        price_new$State, price_new, sum)

# Average the annual rents of all the cities in a County to get average annual rent by County 
price_annual = aggregate(price_sum$"price_new$Price" ~ price_sum$"price_new$County" + 
                           price_sum$"price_new$Year" + price_sum$"price_new$Metro" +
                           price_sum$"price_new$State", price_sum, mean)

# Rename Columns
price_annual = rename(price_annual, 
                      County = "price_sum$\"price_new$County\"",
                      Metro = "price_sum$\"price_new$Metro\"",
                      Year = "price_sum$\"price_new$Year\"", 
                      State = "price_sum$\"price_new$State\"", 
                      Avg_Annual_Rent = "price_sum$\"price_new$Price\"")

# Subset years into separate dataframes 
zillow_2013 = subset(price_annual, substr(Year, 1, 5) == "2013")
zillow_2014 = subset(price_annual, substr(Year, 1, 5) == "2014")
zillow_2015 = subset(price_annual, substr(Year, 1, 5) == "2015")
zillow_2016 = subset(price_annual, substr(Year, 1, 5) == "2016")

# Set census API key
mycensuskey = "b5aa730d1c47276730a6ad4979bdae9095278be8" # http://api.census.gov/data/key_signup.html

# Set variables: Name, Median Household Income and Total Population in past 12 months
myvars <- c("NAME", "B19013_001E", "B01001_001E")

# Download data from census for 2013-2015
# 2016 data not downloading
census_2013= getCensus(name = "acs1", vintage = 2013, key = mycensuskey, vars = myvars, region = "county:*")
census_2014= getCensus(name = "acs1", vintage = 2014, key = mycensuskey, vars = myvars, region = "county:*")
census_2015= getCensus(name = "acs1", vintage = 2015, key = mycensuskey, vars = myvars, region = "county:*")

# Import 2016 from ACS data on factfinder.com 
census_2016_TotalPopulation = read_csv("C:\\Users\\Adimn\\Desktop\\zillow project\\ACS_16_1YR_B01003_TotalPopulation.csv")
census_2016_MedianIncome = read_csv("C:\\Users\\Adimn\\Desktop\\zillow project\\ACS_16_1YR_B19013_MedianIncome.csv")

# Merge Total Population and Median Income by years 
census_2016 = merge(census_2016_TotalPopulation, census_2016_MedianIncome, by = "GEO.display-label")

# Rename Census Columns
census_2013 = rename(census_2013, County = "NAME", Med_Annual_Income = "B19013_001E", County_Population = "B01001_001E")
census_2014 = rename(census_2014, County = "NAME", Med_Annual_Income = "B19013_001E", County_Population = "B01001_001E")
census_2015 = rename(census_2015, County = "NAME", Med_Annual_Income = "B19013_001E", County_Population = "B01001_001E")
census_2016 = rename(census_2016, County = "GEO.display-label", Med_Annual_Income = "HD01_VD01.y", County_Population = "HD01_VD01.x")

# Drop Unwanted Columns
census_2013 <- subset(census_2013, select = -c(county,state))
census_2014 <- subset(census_2014, select = -c(county,state))
census_2015 <- subset(census_2015, select = -c(county,state))
census_2016 <- subset(census_2016, select = c(County,County_Population,Med_Annual_Income))

# Rename Louisiana's Parish to County to match Zillow data
census_2013$County <- gsub('Parish', 'County', census_2013$County)
census_2014$County <- gsub('Parish', 'County', census_2014$County)
census_2015$County <- gsub('Parish', 'County', census_2015$County)
census_2016$County <- gsub('Parish', 'County', census_2016$County)

# Merge Zillow and Census data
zillow_2013_new = merge(zillow_2013, census_2013, by = "County")
zillow_2014_new = merge(zillow_2014, census_2014, by = "County")
zillow_2015_new = merge(zillow_2015, census_2015, by = "County")
zillow_2016_new = merge(zillow_2016, census_2016, by = "County")

# Calculate Annual Rent Expense as % of Household Income
zillow_2013_new$Rent_Income = zillow_2013_new$Avg_Annual_Rent/zillow_2013_new$Med_Annual_Income
zillow_2014_new$Rent_Income = zillow_2014_new$Avg_Annual_Rent/zillow_2014_new$Med_Annual_Income
zillow_2015_new$Rent_Income = zillow_2015_new$Avg_Annual_Rent/zillow_2015_new$Med_Annual_Income
zillow_2016_new$Rent_Income = zillow_2016_new$Avg_Annual_Rent/as.numeric(zillow_2016_new$Med_Annual_Income)

# Combine Datasets
zillow_combined = Reduce(function(x, y) merge(x, y, all=TRUE), 
                         list(zillow_2013_new, zillow_2014_new, zillow_2015_new, zillow_2016_new))

# Calculate percentage change of annual income, population and rent
zillow_combined$Percent_AnnualRent_Change <- Delt(as.numeric(zillow_combined$Avg_Annual_Rent))
zillow_combined$Percent_AnnualRent_Change[which(zillow_combined$Year=="2013"),] <- NA
zillow_combined$Percent_Pop_Change <- Delt(as.numeric(zillow_combined$County_Population))
zillow_combined$Percent_Pop_Change[which(zillow_combined$Year=="2013"),] <- NA
zillow_combined$Percent_AnnualIncome_Change <- Delt(as.numeric(zillow_combined$Med_Annual_Income))
zillow_combined$Percent_AnnualIncome_Change[which(zillow_combined$Year=="2013"),] <- NA

# Add year column to census
census_2013$Year <- 2013
census_2014$Year <- 2014
census_2015$Year <- 2015
census_2016$Year <- 2016

# Combine all annual censuses together
census_combined <- rbind(census_2013,census_2014,census_2015,census_2016)

# Calculate Mean Monthly Rent by County, and remove unwanted columns
price_mean_monthly = aggregate(price_new$Price ~ price_new$County + 
                                 price_new$Month + price_new$Year +
                                 price_new$Metro + price_new$State + price_new$Mon, price_new, mean)

# Rename Columns 
price_mean_monthly = rename(price_mean_monthly, 
                            County = "price_new$County",
                            Metro = "price_new$Metro",
                            Date = "price_new$Month", 
                            State = "price_new$State", 
                            Year = "price_new$Year",
                            Month = "price_new$Mon",
                            Rent = "price_new$Price")

# Create a new dataframe of monthly data with zillow and census combined
monthly_zillow_census <- merge(census_combined,price_mean_monthly,by=c("County","Year"))

# Change Median Annual Income to Monthly Income
monthly_zillow_census$Med_Monthly_Income <- as.numeric(monthly_zillow_census$Med_Annual_Income)/12

# Remove unwanted columns
monthly_zillow_census <- subset(monthly_zillow_census, select = -c(Med_Annual_Income))

# Transform Date column into datetime format
myFun <- function(x, dummyDay = "01", Year = substr(as.character(monthly_zillow_census$Date),5,6)){
  require(lubridate)
  
  x <- ifelse(substr(as.character(monthly_zillow_census$Date), 1, 3) %in% month.abb,
              paste(match(substr(as.character(monthly_zillow_census$Date), 1, 3), month.abb),
                    dummyDay,
                    Year, sep = "-"), x)
  mdy(x)
}

monthly_zillow_census$Date <- myFun(as.character((monthly_zillow_census$Date)))

# Transform Month column from month abbreviation into numbers
monthly_zillow_census$Month <- match(monthly_zillow_census$Month,month.abb)

# Save monthly_zillow_census and zillow_combined dataframes into csv file
write.csv(monthly_zillow_census,"monthly_zillow_census.csv")
write.csv(zillow_combined,"zillow_combined.csv")

