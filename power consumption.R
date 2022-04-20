# load the libraries 
library(RMySQL)
library(dplyr)
library(tibbletime)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggfortify)

# Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
# List the tables contained in the database 
dbListTables(con)

# Using the dbListFields function to learn the attributes associated with the yr_2006 table
dbListFields(con, 'yr_2006')

# Use the dbGetQuery function to download tables 2006 through 2010 with Date, Time and the 3 sub-meter attributes
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2010")

# Investigate each new data frame
# Use str(), summary(), head() with each data frame
str(yr_2010)

summary(yr_2006)
head(yr_2010)

# Combine tables into one dataframe using dplyr and investigate the new dataframe 
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)
str(newDF)
summary(newDF)
head(newDF)

# Combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF, paste(newDF$Date,newDF$Time), 
              stringsAsFactors=FALSE)
# Give the new attribute a header name 
colnames(newDF)[6] <-"DateTime"
# Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]

# Add the time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
# Convert DateTime from character to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S", origin = "2007-01-01 01:00:00")

# convert the dataframe to tibbletime 
newDF <- as_tbl_time(newDF, index = DateTime)
head(newDF)

# Create "year" attribute with lubridate
newDF$year <- year(newDF$DateTime)
# quarter, month, week, weekday, day, hour and minute
newDF$quarter <- quarter(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekday <- weekdays(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)

# initial exploration 
summary(newDF)
sd(newDF$Sub_metering_1)
sd(newDF$Sub_metering_2)
sd(newDF$Sub_metering_3)
mode(newDF$Sub_metering_1)
sum(newDF$Sub_metering_1)
sum(newDF$Sub_metering_2)
sum(newDF$Sub_metering_3)

# Visualize the Data
# Reduce the granularity to every 30 minutes
newDF_30min <- as_period(newDF, '30 minute')
# Subset the second week of 2008 
houseWeek <- filter(newDF, year == 2008 & week == 2)
# Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

# Visualize a Single Day with Plotly
# Reducing Granularity
# Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(newDF, year == 2008 & month == 1 & day == 9 
                     & (minute == 0 | minute == 10 | minute == 20 | 
                          minute == 30 | minute == 40 | minute == 50))
houseDay10$minute
# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Create a visualization with plotly for 2nd Week of 2008
newDF_40min <- as_period(newDF, '40 minute')
houseWeek40 <- filter(newDF_40min, year == 2008 & week == 2)
houseWeek40
plot_ly(houseWeek40, x = ~houseWeek40$DateTime, y = ~houseWeek40$Sub_metering_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek40$Sub_metering_2, name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~houseWeek40$Sub_metering_3, name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, 2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# choose monthly time period and use 60 mins interval
newDF_1h <- as_period(newDF, "1 h")
newDF_1h
houseMonth1h <- filter(newDF_1h, year == 2008, month == 1 )
houseMonth1h
plot_ly(houseMonth1h, x = ~houseMonth1h$DateTime, y = ~houseMonth1h$Sub_metering_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth1h$Sub_metering_2, name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~houseMonth1h$Sub_metering_3, name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power Consumption Jan,2008",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (watt-hours)"))

# Time Series Analysis
# Prepare to analyze the data
# Subset to one observation per week on Mondays at 8:00pm 
# for 2007, 2008 and 2009
house070809weekly <- filter(newDF, weekday == 'Monday' & hour == 20 & minute == 0)
house070809weekly
# Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, 
                         frequency=52, start=c(2007,1))
tsSM3_070809weekly
# Produce time series plots
# Plot sub-meter 3 with autoplot (you may need to install these packages)
# Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 3, Mondays, 20:00 ")

# Create a TS plot for Sub-meter 3 on Saturdays at 20:00
house070809weeklySat <- filter(newDF, weekday == 'Saturday' & hour == 20 & minute == 0)
house070809weeklySat
tsSM3_070809weeklySat <- ts(house070809weeklySat$Sub_metering_3, 
                            frequency=52, start=c(2007,1))
tsSM3_070809weeklySat
plot.ts(tsSM3_070809weekly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 3, Saturdays, 20:00 ")

# Create a TS plot for Sub-meter 1 on Saturdays at 20:00
house070809weeklySat20 <- filter(newDF, weekday == 'Saturday' & hour == 20 &
                                   minute == 0)
tsSM1_070809weeklySat20 <- ts(house070809weeklySat$Sub_metering_1,
                              frequency = 52, start = c(2007,1))
plot.ts(tsSM1_070809weeklySat20, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 1, Saturdays, 20:00 ")

# Create a TS plot for Sub-meter 2 in May, 2007, 2008, 2009
house07Monthly <- filter(newDF_1h, year == 2007 & month == 5 )
tsSM2_07Monthly <- ts(house07Monthly$Sub_metering_2,
                      frequency = 24)
plot.ts(tsSM2_07Monthly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 2, May, 2007")
# May, 2008
house08Monthly <- filter(newDF_1h, year == 2008 & month == 5 )
tsSM2_08Monthly <- ts(house08Monthly$Sub_metering_2,
                      frequency = 24)
plot.ts(tsSM2_08Monthly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 2, May, 2008 ")
# May, 2009
house09Monthly <- filter(newDF_1h, year == 2009 & month == 5 )
tsSM2_09Monthly <- ts(house09Monthly$Sub_metering_2,
                      frequency = 24)
plot.ts(tsSM2_09Monthly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 2, May, 2009 ")


