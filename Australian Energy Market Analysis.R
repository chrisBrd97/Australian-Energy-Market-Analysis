# OUTLINE OF ANALYSIS -----------------------------------------------------

# This analysis has been conducted on a data set of electricity demand and
# prices for five Australian states (NSW, QLD, SA, VIC & TAS) in 2020. There are four
# distinct sections of the analysis below, the first looks at boxplots to display 
# total demand and demand per capita distributions for each state. The second is 
# a line graph tracking total demand and demand per capita for each state over the 
# course of a year. The third section is scatterplots of each states demand per 
# capita compared to the average price (outliers have not been removed from this 
# data). The final section is a line graph of the hourly demand per capita of each 
# state. Following all this, numerical summaries for the boxplots and scatterplots
# have also been provided in the form of a five number summary and correlation
# analysis.

# LOAD LIBRARIES -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(gridExtra)
library(psych)


# DATA PREPARATION --------------------------------------------------------

# load data files for analysis and combine into single dataframe
Australian_Energy_Data <- list.files(pattern = "\\.csv$") %>%
  lapply(read_csv) %>%
  bind_rows

# remove unused column for analysis "PERIODTYPE"
Australian_Energy_Data <- Australian_Energy_Data[,-5]

# remove rows with data from year 2021 as only analysing 2020
Australian_Energy_Data <- Australian_Energy_Data[!grepl('2021/01/01 00:00:00', Australian_Energy_Data$SETTLEMENTDATE),]

# add additional column for state population
Australian_Energy_Data$POPULATION <- case_when(Australian_Energy_Data$REGION == "NSW1" ~ 8176400,
                                               Australian_Energy_Data$REGION == "TAS1" ~ 542000,
                                               Australian_Energy_Data$REGION == "SA1" ~ 1771700,
                                               Australian_Energy_Data$REGION == "VIC1" ~ 6648600,
                                               Australian_Energy_Data$REGION == "QLD1" ~ 5206400)

# add column dividing total demand by population to find "demand per capita"
Australian_Energy_Data$DEMANDPERCAPITA <- Australian_Energy_Data$TOTALDEMAND/Australian_Energy_Data$POPULATION

# convert settlement date to date-time format and add new column
Australian_Energy_Data$DATETIME <- as.POSIXlt(Australian_Energy_Data$SETTLEMENTDATE, format = "%Y/%m/%d %H")

# round date-time to daily time unit and add new column
Australian_Energy_Data$DAY <- floor_date(Australian_Energy_Data$DATETIME, "day")

# round date-time to hourly time unit and add new column
Australian_Energy_Data$HOUR <- floor_date(Australian_Energy_Data$DATETIME, "hour")

# aggregate daily total demand, demand per capita and price into new dataframe
Daily_Data <- Australian_Energy_Data %>%
  group_by(DAY, REGION) %>%
  summarise(sum_total_demand = sum(TOTALDEMAND), 
  sum_demand_per_capita = sum(DEMANDPERCAPITA), 
  average_price = mean(RRP))
#convert column "DAY" into class Date
Daily_Data$DAY <- as.Date(Daily_Data$DAY)

# aggregrate hourly total demand and demand per capita into a new dataframe 
Hourly_Data <- Australian_Energy_Data %>%
  group_by(hour(HOUR), REGION) %>%
  summarise(sum_total_demand = sum(TOTALDEMAND), 
            sum_demand_per_capita = sum(DEMANDPERCAPITA))
Hourly_Data$HOUR <- Hourly_Data$`hour(HOUR)`


# DATA ANALYSIS -----------------------------------------------------------

# boxplot of daily total demand for each state
boxplot_total_demand <- ggplot(data = Daily_Data, aes(x=as.character(REGION), y=sum_total_demand)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Distribution of Total Electricity Demand by State (2020)", x="State", y="Electricity Demand (MW)") + 
  theme(plot.title = element_text(hjust = 0.5))

# boxplot of daily demand per capita for each state
boxplot_demand_per_capita <- ggplot(Daily_Data, aes(x=as.character(REGION), y=sum_demand_per_capita)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Distribution of Electricity Demand per Capita by State (2020)", x="State", y="Electricity Demand (MW)") + 
  theme(plot.title = element_text(hjust = 0.5))

# create grid to display both boxplot graphs simultaneously
gridExtra::grid.arrange(boxplot_total_demand, boxplot_demand_per_capita, ncol=1, nrow=2)


# line graph of daily total demand for each state by day
linegraph_daily_demand <- ggplot(Daily_Data, aes(x = DAY, y = sum_total_demand, color = REGION)) +
  geom_line(lwd = 1) + scale_x_date(date_labels = "%Y/%m/%d") +
  labs(title="Electricity Demand per Day (2020)", x="Date", y="Electricity Demand (MW)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "State"))

# line graph of daily demand per capita for each state by day
linegraph_daily_demand_per_capita <- ggplot(Daily_Data, aes(x = DAY, y = sum_demand_per_capita, color = REGION)) +
  geom_line(lwd = 1) + scale_x_date(date_labels = "%Y/%m/%d") +
  labs(title="Electricity Demand per Capita per Day (2020)", x="Date", y="Electricity Demand (MW)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "State"))

# create grid to display both line graphs simultaneously
gridExtra::grid.arrange(linegraph_daily_demand, linegraph_daily_demand_per_capita, ncol=1, nrow=2)
## Line graphs not showing in plots unless the zoom or refresh button is clicked ##


# scatterplots of correlation between average price and demand per capita for each state

# South Australia
with(Daily_Data[Daily_Data$REGION == "SA1", ], plot(sum_demand_per_capita, average_price,
                                                    cex=1,
                                                    abline(lm(average_price ~ sum_demand_per_capita), col = "red"),
                                                    col="black",
                                                    ylab="Average Price", xlab="Electricity Demand (MW)",
                                                    main="Demand per Capita vs Average Price (SA)"))
# New South Wales
with(Daily_Data[Daily_Data$REGION == "NSW1", ], plot(sum_demand_per_capita, average_price,
                                                    cex=1,
                                                    abline(lm(average_price ~ sum_demand_per_capita), col = "red"),
                                                    col="black",
                                                    ylab="Average Price", xlab="Electricity Demand (MW)",
                                                    main="Demand per Capita vs Average Price (NSW)"))
# Queensland
with(Daily_Data[Daily_Data$REGION == "QLD1", ], plot(sum_demand_per_capita, average_price,
                                                    cex=1,
                                                    abline(lm(average_price ~ sum_demand_per_capita), col = "red"),
                                                    col="black",
                                                    ylab="Average Price", xlab="Electricity Demand (MW)",
                                                    main="Demand per Capita vs Average Price (QLD)"))
# Victoria
with(Daily_Data[Daily_Data$REGION == "VIC1", ], plot(sum_demand_per_capita, average_price,
                                                    cex=1,
                                                    abline(lm(average_price ~ sum_demand_per_capita), col = "red"),
                                                    col="black",
                                                    ylab="Average Price", xlab="Electricity Demand (MW)",
                                                    main="Demand per Capita vs Average Price (VIC)"))
# Tasmania
with(Daily_Data[Daily_Data$REGION == "TAS1", ], plot(sum_demand_per_capita, average_price,
                                                    cex=1,
                                                    abline(lm(average_price ~ sum_demand_per_capita), col = "red"),
                                                    col="black",
                                                    ylab="Average Price", xlab="Electricity Demand (MW)",
                                                    main="Demand per Capita vs Average Price (TAS)"))

# line graph of hourly total demand for each state 
linegraph_hourly_demand <- ggplot(Hourly_Data, aes(x = HOUR, y = sum_total_demand, color = REGION)) +
  geom_line(lwd = 1) +
  labs(title="Electricity Demand by Hour (2020)", x="Hour", y="Electricity Demand (MW)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "State"))

# line graph of hourly demand per capita for each state
linegraph_hourly_demand_per_capita <- ggplot(Hourly_Data, aes(x = HOUR, y = sum_demand_per_capita, color = REGION)) +
  geom_line(lwd = 1) +
  labs(title="Electricity Demand per Capita by Hour (2020)", x="Hour", y="Electricity Demand (MW)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "State"))

# create grid to display both line graphs simultaneously
gridExtra::grid.arrange(linegraph_hourly_demand, linegraph_hourly_demand_per_capita, ncol=1, nrow=2)



## Numerical Summaries ##

# 5 Number Numerical Summary for Daily Total Demand by State #
print(tapply(Daily_Data$sum_total_demand, Daily_Data$REGION, summary))

# 5 Number Numerical Summary for Daily Demand per Capita by State #
print(tapply(Daily_Data$sum_demand_per_capita, Daily_Data$REGION, summary))

# Correlation (r) of Average Price and Demand per Capita by State #
print(correlation_summary <- Daily_Data %>%
  group_by(REGION) %>%
  summarise(cor=cor(sum_demand_per_capita, average_price)))
