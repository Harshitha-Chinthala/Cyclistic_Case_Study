#Load the packages
library(tidyverse)
library(readr)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(dplyr)

#Set the working directory
setwd("C:/Users/chint/OneDrive/Documents/Cyclistic_Case_Study/Raw_Data")
getwd()

#List all CSV files in the Raw_Data folder
file_list <- list.files(pattern="2023.*.csv")

#Read and merge all the files into one data set
cyclistic_data <- file_list %>% 
  map_df(~ read.csv(.))

#View data set
View(cyclistic_data)

#Checking missing values for each column
cyclistic_data[cyclistic_data == ""] <- NA
cyclistic_data[cyclistic_data == "NULL"] <- NA
colSums(is.na(cyclistic_data))

#Converting started_at and ended_at to proper datetime format
cyclistic_data$started_at <- ymd_hms(cyclistic_data$started_at)
cyclistic_data$ended_at <- ymd_hms(cyclistic_data$ended_at)

#Calculating ride_length
cyclistic_data$ride_length <- as.numeric(cyclistic_data$ended_at - cyclistic_data$started_at, units = "mins")

#View summary statistics for ride_length
summary(cyclistic_data$ride_length)

#Extract the day_of_week from the started_at
cyclistic_data$day_of_week <- weekdays(cyclistic_data$started_at)
cyclistic_data$day_of_week <- factor(cyclistic_data$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#Display the count of rides for each day_of_week
table(cyclistic_data$day_of_week)

#Remove negative or zero ride_length
cyclistic_data <- cyclistic_data %>% 
  filter(ride_length > 0)

#Check the updated data
summary(cyclistic_data$ride_length)

#Extract hour
cyclistic_data$hour <- format(cyclistic_data$started_at,"%H")

#Extract month
cyclistic_data$month <- format(cyclistic_data$started_at,"%B")
cyclistic_data$month <- factor(cyclistic_data$month, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))

#Calculate total rides by user type
table(cyclistic_data$member_casual)

#Calculate total rides by bike type
table(cyclistic_data$rideable_type)

#Calculate average ride length for each user type
cyclistic_data %>% 
  group_by(member_casual) %>%
  summarise(avg_ride_length = mean(ride_length))

#Calculate average ride_length by user type and weekday
cyclistic_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(avg_ride_length = mean(ride_length, na.rm=TRUE))

#Count total rides for each day_of_week
cyclistic_data %>% 
  group_by(day_of_week) %>%
  summarise(total_rides = n())

#Count total rides by user type and weekday
cyclistic_data %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(total_rides = n())

#Count total rides by hour
table(cyclistic_data$hour)

#Count total rides by month
cyclistic_data %>%
  group_by(month) %>%
  summarise(total_rides = n())

#Bike type preference by user type
cyclistic_data %>%
  group_by(member_casual,rideable_type) %>%
  summarise(total_rides = n()) %>%
  arrange(member_casual,desc(total_rides))

#Most popular start stations
cyclistic_data %>%
  filter(start_station_name != "") %>% 
  group_by(start_station_name) %>%
  summarise(total_rides=n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)

#Most popular end stations
cyclistic_data %>%
  filter(end_station_name != "") %>% 
  group_by(end_station_name) %>%
  summarise(total_rides=n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)

#Save the cleaned data set
#write_csv(cyclistic_data,"Cyclistic_Cleaned_Data.csv")

#Check if the file is ready
#file.exists("Cyclistic_Cleaned_Data.csv")

#Randomly sample 4.5M rows
#set.seed(123)
#cyclistic_sample <- cyclistic_data %>%
# sample_n(4500000)
#write_csv(cyclistic_sample,"Cyclistic_Sample_Data.csv")

#Bar chart to compare total rides between Members and Casual Riders
ggplot(data=cyclistic_data) +
  geom_bar(mapping=aes(x=member_casual,fill=member_casual)) +
  labs(title="Total Rides by User Type",x="User Type",y="Count of Rides") +
  scale_y_continuous(labels=scales::comma)

#Bar chart to visualize the distribution of different bike types used
ggplot(data=cyclistic_data) +
  geom_bar(mapping=aes(x=rideable_type,fill=rideable_type)) +
  labs(title="Total Rides by Bike Type",fill="Bike Type")+
  scale_y_continuous(labels=scales::comma)

#Bar chart to compare average ride duration for each user type
ggplot(data=cyclistic_data) +
  geom_bar(mapping=aes(x=member_casual,y=ride_length,fill=member_casual),stat='summary',fun="mean")+
  labs(title="Average Ride Length by User Type",x="User Type",y="Avg Ride Length (mins)")

#Bar chart to compare average ride length for each weekday by user type
ggplot(data=cyclistic_data) +
  geom_bar(mapping=aes(x=day_of_week,y=ride_length,fill=member_casual),stat="summary",fun="mean",position="dodge") +
  labs(title="Average Ride Length by User Type & Weekday",x="Day of the Week",y="Avg Ride Length(mins)")

#Bar chart to display the number of rides taken on each weekday
ggplot(data=cyclistic_data) +
  geom_bar(mapping=aes(x=day_of_week,fill=member_casual),position="dodge") +
  labs(title="Total Rides by the Day of the Week",x="Day of the Week",y="Count of Rides")

#Stacked bar chart to show ride distribution across weekdays for each user type
ggplot(data=cyclistic_data) +
  geom_bar(mapping=aes(x=day_of_week,fill=member_casual))+
  labs(title="Total Rides by User Type & Weekday",x="Day of Week",y="Count of Rides")

#Line chart to visualize ride activity across different hours of the day
ggplot(data=cyclistic_data) +
  geom_line(mapping=aes(x=hour,group=member_casual,color=member_casual),stat="count",linewidth=1.5) +
  labs(title="Total Rides by Hour",x="Hour of the Day",y="Count of Rides") +
  scale_y_continuous(labels=scales::comma)

#Line chart to show seasonal trends in ride activity across months
ggplot(data=cyclistic_data) +
  geom_line(mapping=aes(x=month,group=member_casual,color=member_casual),stat="count",linewidth=1.5) +
  labs(title="Total Rides by Month",x="Month",y="Count of Rides") +
  scale_y_continuous(labels=scales::comma) +
  theme(axis.text.x = element_text(angle=45,hjust=1))
