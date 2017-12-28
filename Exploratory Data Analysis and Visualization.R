library(readr)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(censusapi)
library(quantmod)
library(lubridate)
library(ggplot2)

# Deal with zillow_combined.csv
# Calculate Mean Annual Rent by Metro
zillow_rent_metro = aggregate(zillow_combined$Avg_Annual_Rent ~ zillow_combined$Metro + 
                                zillow_combined$State + zillow_combined$Year, zillow_combined, mean)

# Calculate Mean Annual Income by Metro
zillow_income_metro = aggregate(as.numeric(zillow_combined$Med_Annual_Income) ~ zillow_combined$Metro + 
                                  zillow_combined$State + zillow_combined$Year, zillow_combined, mean)

# Calculate Population by Metro
zillow_population_metro = aggregate(as.numeric(zillow_combined$County_Population) ~ zillow_combined$Metro + 
                                      zillow_combined$State + zillow_combined$Year, zillow_combined, sum)

# Create a new metro dataframe of the above three combined
zillow_metro <- cbind(zillow_rent_metro,zillow_income_metro$'as.numeric(zillow_combined$Med_Annual_Income)',zillow_population_metro$'as.numeric(zillow_combined$County_Population)')

# Rename Columns 
zillow_metro = rename(zillow_metro, 
                      Metro = "zillow_combined$Metro",
                      State = "zillow_combined$State", 
                      Year = "zillow_combined$Year",
                      Med_Annual_Income = 'zillow_income_metro$"as.numeric(zillow_combined$Med_Annual_Income)"',
                      Avg_Annual_Rent = "zillow_combined$Avg_Annual_Rent",
                      Population = 'zillow_population_metro$"as.numeric(zillow_combined$County_Population)"')

#see the zillow_metro dataframe with population descending ranking
metro_rank <- zillow_metro %>% 
  arrange(desc(Population))
#find out New York is classified into two parts:New York,NY and New York,NJ

#combine ny,ny and ny,nj
ny_ny <- metro_rank[c(9:12),]
ny_nj <- metro_rank[c(17:20),]
ny <- merge(ny_ny,ny_nj,by=c("Year"))
ny$Avg_Annual_Rent <- (ny$Avg_Annual_Rent.x+ny$Avg_Annual_Rent.y)/2
ny$Med_Annual_Income <- (ny$Med_Annual_Income.x+ny$Med_Annual_Income.y)/2
ny$Population <- ny$Population.x+ny$Population.y
ny <- ny[,-c(4:11)]
ny = rename(ny,
            Metro="Metro.x",
            State="State.x")

#comine ny into metro_rank
metro_rank <- rbind(metro_rank,ny)
metro_rank <- metro_rank[-c(9:12,17:20),]
metro_rank <- metro_rank %>%
  arrange(desc(Population))

#select top20 metro
metro_top20 <- metro_rank[c(1:79,86),]

#plot line chart of annual rent trend of metro top20 from 2013-2016
cbPalette <- c("#999999","#000000","#E69F00","#56B4E9","#009E73","#FF6600","#0072B2","#D55E00","#CC79A7","#66CC99",
               "#9999CC","#CC6666","#FF0000","#00FF00","#FFCC00","#006633","#FF9999","#660066","#FF6666","#666600")

ggplot(metro_top20,aes(x=as.numeric(Year),y=Avg_Annual_Rent,col=Metro)) +
  geom_line() +
  geom_point(size=3) +
  scale_color_manual(values=cbPalette)

# Deal with monthly_zillow_census.csv
# Calculate Mean Annual Rent by Metro
monthly_rent_metro = aggregate(monthly_zillow_census$Rent ~ monthly_zillow_census$Metro + 
                                 monthly_zillow_census$State + monthly_zillow_census$Year + monthly_zillow_census$Date + monthly_zillow_census$Month, monthly_zillow_census, mean)

# Rename Columns 
monthly_rent_metro = rename(monthly_rent_metro, 
                            Metro = "monthly_zillow_census$Metro",
                            State = "monthly_zillow_census$State", 
                            Year = "monthly_zillow_census$Year",
                            Date = "monthly_zillow_census$Date",
                            Month = "monthly_zillow_census$Month",
                            Rent = "monthly_zillow_census$Rent")

# Select only the top20 metro areas for analysis here
monthly_rent_metro_top20 <- merge(metro_top20,monthly_rent_metro,by=c("Metro","Year","State"))
monthly_rent_metro_top20 <- monthly_rent_metro_top20[,-c(4:6)]

# Plot line chart of monthly rent trend of metro top20 from 2013-2016
ggplot(monthly_rent_metro_top20,aes(x=Date,y=Rent,col=Metro)) +
  geom_line() +
  geom_point(size=2) +
  scale_color_manual(values=cbPalette)

# Select rows of year 2013 and 2016
metro_top20_13 <- metro_top20[which(metro_top20$Year=="2013"),]
metro_top20_16 <- metro_top20[which(metro_top20$Year=="2016"),]
metro_top20_1316 <- rbind(metro_top20_13,metro_top20_16)

# Change row order for % calculation
metro_top20_1316 <- metro_top20_1316 %>%
  arrange(Population)
metro_top20_1316 <- metro_top20_1316[c(1:9,11,10,12:21,24,22,26,23,27,25,28,29:34,36,35,37:40),]

# Calculate percentage change of annual income, population and rent
metro_top20_1316$Percent_Rent_Change <- Delt(as.numeric(metro_top20_1316$Avg_Annual_Rent))
metro_top20_1316$Percent_Rent_Change[which(metro_top20_1316$Year=="2013"),] <- NA
metro_top20_1316$Percent_Pop_Change <- Delt(as.numeric(metro_top20_1316$Population))
metro_top20_1316$Percent_Pop_Change[which(metro_top20_1316$Year=="2013"),] <- NA
metro_top20_1316$Percent_Income_Change <- Delt(as.numeric(metro_top20_1316$Med_Annual_Income))
metro_top20_1316$Percent_Income_Change[which(metro_top20_1316$Year=="2013"),] <- NA

# Format dismal places
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# Bar chart of rent change from 2013 to 2016 of top20 metro
ggplot(metro_top20_1316,aes(x=Metro,y=Percent_Rent_Change)) +
  geom_bar(stat="identity",fill=cbPalette) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  geom_text(aes(label=specify_decimal(metro_top20_1316$Percent_Rent_Change,3)), vjust=1.6, color="white", size=4)

# Bar chart of income change from 2013 to 2016 of top20 metro
ggplot(metro_top20_1316,aes(x=Metro,y=Percent_Income_Change)) +
  geom_bar(stat="identity",fill=cbPalette) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  geom_text(aes(label=specify_decimal(metro_top20_1316$Percent_Income_Change,3)), vjust=1.6, color="white", size=4)

# Reshape data from wide to long format
metro_rent_income = gather(data=metro_top20_1316,"Category","Number",c(7,9),na.rm=T)
metro_rent_income_pop = gather(data=metro_top20_1316,"Category","Number",7:9,na.rm=T)

# Plot bar chart of income and rent change for metro top20
ggplot(metro_rent_income,aes(x=Metro,y=Number,fill=Category)) +
  geom_bar(stat="identity",position=position_dodge()) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  geom_text(aes(label=specify_decimal(metro_rent_income$Number,3)), vjust=-0.3, color="black", position=position_dodge(0.9),size=3)

# Plot bar chart of income,rent and population change for metro top20
ggplot(metro_rent_income_pop,aes(x=Metro,y=Number,fill=Category)) +
  geom_bar(stat="identity",position=position_dodge()) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  geom_text(aes(label=specify_decimal(metro_rent_income_pop$Number,3)), vjust=-0.3, color="black", position=position_dodge(0.9),size=3)
