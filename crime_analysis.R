# Load library
library(tidyverse)
library(magrittr)
library(dplyr)
library(lubridate)
library(scales)
library(viridis)
 
type=cols( `CASE#` = col_character(),
           `DATE  OF OCCURRENCE` = col_datetime(format="%m/%d/%Y %I:%M:%S %p"),
           BLOCK = col_factor(),
           IUCR = col_factor(),
           `PRIMARY DESCRIPTION` = col_factor(),
           `SECONDARY DESCRIPTION` = col_factor(),
           `LOCATION DESCRIPTION` = col_factor(),
           ARREST = col_factor(),
           DOMESTIC = col_factor(),
           BEAT = col_factor(),
           WARD = col_factor(),
           `FBI CD` = col_factor(),
           `X COORDINATE` = col_double(),
           `Y COORDINATE` = col_double(),
           LATITUDE = col_double(),
           LONGITUDE = col_double(),
           LOCATION = col_character()
)
# Download data from internet
url.data <- "https://data.cityofchicago.org/api/views/x2n5-8w5q/rows.csv?accessType=DOWNLOAD"

# Read data
crime_raw <- read_csv(url.data, na='',col_types = type)

# Fix column names
names(crime_raw)<-str_to_lower(names(crime_raw)) %>%
  str_replace_all(" ","_") %>%
  str_replace_all("__","_") %>%
  str_replace_all("#","_num")

# Print data
crime_raw

# Understanding the data fields
str(crime_raw)

# Summarize the data
summary(crime_raw)

# Identify duplicate identifiers
crime_raw %>%
  group_by(case_num) %>%
  mutate(count=n()) %>%
  filter(count>1)

# Get row names for display
getrow<-t(filter(crime_raw,case_num=='JC438604'))

# Create tibble for example duplicate record
JC438604<-as_tibble(t(filter(crime_raw,case_num=='JC438604')),.name_repair=NULL,validate=NULL)

# add row names and reorganize the duplicate for display
JC438604 %>% 
  mutate(Variable=rownames(getrow)) %>%
  rename(Row1=V1,Row2=V2,Row3=V3) %>%
  select(Variable, Row1, Row2)

# Remove duplicates
crime_no_dup<-filter(distinct(crime_raw,case_num,.keep_all=TRUE))

# Check for duplicate values 
crime_no_dup %>%
  group_by(case_num) %>%
  summarize(count=n()) %>%
  filter(count>1)

crime_no_dup %>% 
  select(date_of_occurrence) %>% 
  head()
crime_no_dup %>% 
  select(date_of_occurrence) %>% 
  tail()

# Remove timestamp from datetime and place in separate column
crime_clean<-crime_no_dup %>%
  mutate(time=hms::as_hms(hour(date_of_occurrence)*60+minute(date_of_occurrence)),
         # Remove timestamp from datetime and place in separate column
         date=date(date_of_occurrence), 
         # Separate date part from date time
         time_group=cut(as.numeric(time),breaks=c(0,6*60,12*60,18*60,23*60+59),labels=c("00-06","06-12","12-18","18-00"),include.lowest = TRUE))

crime_clean %>% select(case_num, date_of_occurrence, date, time, time_group)
crime_clean %>% group_by(time_group) %>% summarize(count=n())

crime_clean <- crime_clean %>%
  mutate(
    day=wday(date,label=TRUE,abbr=TRUE),
    month=month(date,label=TRUE,abbr=TRUE)
  )

crime_clean %>% select(case_num, date_of_occurrence, day, month)

# Specific crime types
(t<-crime_clean %>% 
    group_by(primary_description) %>%
    summarize(count=n()) %>%
    arrange(desc(count)))

# Create general categories for crime 
crime_clean<-crime_clean %>%
  mutate(
    crime=fct_recode(primary_description,
                     "DAMAGE"="CRIMINAL DAMAGE",
                     "DRUG"="NARCOTICS",
                     "DRUG"="OTHER NARCOTIC VIOLATION",
                     "FRAUD"="DECEPTIVE PRACTICE",
                     "MVT"="MOTOR VEHICLE THEFT",
                     "NONVIOLENT"="LIQUOR LAW VIOLATION",
                     "NONVIOLENT"="CONCEALED CARRY LICENSE VIOLATION",
                     "NONVIOLENT"="STALKING",
                     "NONVIOLENT"="INTIMIDATION",
                     "NONVIOLENT"="GAMBLING",
                     "NONVIOLENT"="OBSCENITY",
                     "NONVIOLENT"="PUBLIC INDECENCY",
                     "NONVIOLENT"="INTERFERENCE WITH PUBLIC OFFICER",
                     "NONVIOLENT"="PUBLIC PEACE VIOLATION",
                     "NONVIOLENT"="NON-CRIMINAL",
                     "OTHER"="OTHER OFFENSE",
                     "SEX"="HUMAN TRAFFICKING",
                     "SEX"="CRIMINAL SEXUAL ASSAULT",
                     "SEX"="SEX OFFENSE",
                     "SEX"="CRIM SEXUAL ASSAULT",
                     "SEX"="PROSTITUTION",
                     "TRESSPASS"="CRIMINAL TRESPASS",
                     "VIOLENT"="KIDNAPPING",
                     "VIOLENT"="WEAPONS VIOLATION",
                     "VIOLENT"="OFFENSE INVOLVING CHILDREN"
    ),
    crime_type=fct_recode(crime,
                          "VIOLENT"="SEX",
                          "VIOLENT"="ARSON",
                          "VIOLENT"="ASSAULT",
                          "VIOLENT"="HOMICIDE",
                          "VIOLENT"="VIOLENT",
                          "VIOLENT"="BATTERY",
                          "NONVIOLENT"="BURGLARY",
                          "NONVIOLENT"="DAMAGE",
                          "NONVIOLENT"="DRUG",
                          "NONVIOLENT"="FRAUD",
                          "NONVIOLENT"="MVT",
                          "NONVIOLENT"="NONVIOLENT",
                          "NONVIOLENT"="ROBBERY",
                          "NONVIOLENT"="THEFT",
                          "NONVIOLENT"="TRESSPASS",
                          "NONVIOLENT"="OTHER"
    ) 
    # Further combination into violent and non-violent crime types
  )
crime_clean %>%
  group_by(crime) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

crime_clean %>%
  group_by(crime_type) %>%
  summarize(count=n()) %>%
  arrange(count)


#Visualizations
# Frequency of crime
crime_clean %>% 
  group_by(crime) %>%
  summarise(count=n()) %>%
  ggplot(aes(x = reorder(crime,count), y = count)) +
  geom_bar(stat = "identity", fill = "#756bb1") +
  labs(x ="Crimes", y = "Number of crimes", title = "Crimes in Chicago") + 
  scale_y_continuous(label = comma) +
  coord_flip()

# Time of day
crime_clean %>%
  ggplot(aes(x = time_group)) +
  geom_bar(fill = "#756bb1") +
  labs(x = "Time of day", y= "Number of crimes", title = "Crimes by time of day")

# Day of week
crime_clean %>%
  ggplot(aes(x = day)) +
  geom_bar(fill = "#756bb1") +
  labs(x = "Day of week", y = "Number of crimes", title = "Crimes by day of week")

# Month
crime_clean %>%
  ggplot(aes(x = month)) +
  geom_bar(fill = "#756bb1") +
  labs(x = "Month", y = "Number of crimes", title = "Crimes by month")

crime_clean %>%
  group_by(crime,time_group) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=crime, y=time_group)) +
  geom_tile(aes(fill=count)) +
  labs(x="Crime", y = "Time of day", title="Theft occurs most often between noon and 6pm") +
  scale_fill_viridis_c("Number of Crimes",label=comma) +
  coord_flip()

# Crimes by day of the week
crime_clean %>%
  group_by(crime,day) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=crime, y=day)) +
  geom_tile(aes(fill=count)) +
  labs(x="Crime", y = "Day of week", title="Battery is more prevelant on Sundays") +
  scale_fill_viridis_c("Number of Crimes",label=comma) +
  coord_flip()

# Crimes by month
# A third way of aggregating data is using the summaryBy function from the doBy package
crime_clean %>%
  group_by(crime,month) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=crime, y=month)) +
  geom_tile(aes(fill=count)) +
  labs(x="Crime", y = "Month of year", title="Summer is popular for crimes") +
  scale_fill_viridis_c("Number of Crimes",label=comma) +
  coord_flip()






