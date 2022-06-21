#Cyclistic

The goal of the project is to design marketing strategies aimed at converting casual riders into annual members. In order to do that, in this analysis, we will look into how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics.

The analysis is divided into 5 steps from setting up , Here are the steps:

STEP 1: Setting up libraries and working directory
In this analysis I just use some common libraries, check my working directory then set it to the directory I want.

```{r}

library(tidyverse)
library(lubridate)  
library(ggplot2)
getwd()
setwd("C:/Users/ASUS/Documents/GitHub/CS-Cyclistic")


```

STEP 2: Importing data

I import all tables of trip data in 12 months of 2021.

```{r include=FALSE}
m1.2021 <- read_csv("202101-divvy-tripdata.csv")
m2.2021 <- read_csv("202102-divvy-tripdata.csv")
m3.2021 <- read_csv("202103-divvy-tripdata.csv")
m4.2021 <- read_csv("202104-divvy-tripdata.csv")
m5.2021 <- read_csv("202105-divvy-tripdata.csv")
m6.2021 <- read_csv("202106-divvy-tripdata.csv")
m7.2021 <- read_csv("202107-divvy-tripdata.csv")
m8.2021 <- read_csv("202108-divvy-tripdata.csv")
m9.2021 <- read_csv("202109-divvy-tripdata.csv")
m10.2021 <- read_csv("202110-divvy-tripdata.csv")
m11.2021 <- read_csv("202111-divvy-tripdata.csv")
m12.2021 <- read_csv("202112-divvy-tripdata.csv")
```


Compare column names each of the files to make sure that they are all the same between tables.

```{r}
colnames(m1.2021)
colnames(m2.2021)
colnames(m3.2021)
colnames(m4.2021)
colnames(m5.2021)
colnames(m6.2021)
colnames(m7.2021)
colnames(m8.2021)
colnames(m9.2021)
colnames(m10.2021)
colnames(m11.2021)
colnames(m12.2021)
```


Inspect the dataframes and look for incongruencies.

```{r}
str(m1.2021)
str(m2.2021)
str(m3.2021)
str(m4.2021)
str(m5.2021)
str(m6.2021)
str(m7.2021)
str(m8.2021)
str(m9.2021)
str(m10.2021)
str(m11.2021)
str(m12.2021)
```


Then I stack individual month’s data frames into one big data frame, which is “all_trips”.

```{r}
all_trips <- bind_rows(m1.2021, m2.2021, m3.2021, m4.2021, m5.2021, m6.2021, m7.2021, m8.2021, m9.2021, m10.2021, m11.2021, m12.2021)
```


STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

Apparently, my first step is to inspect the new table that has been created. See if there is any errors or inconsistencies, and what I can transform to prepare for my analysis.

```{r}
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
```


Afterward, there are some transformations I want to make:

  1. Adding some additional columns of data – such as day, month, year – that provide additional opportunities to aggregate the data.

  2. Adding a calculated field for length of ride since the data does not have the column for trip duration.

Therefore, I add columns that list the date, month, day, and year of each ride. This will allow us to aggregate ride data for each month, day, or year.
```{r}
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
```

After that, I add a “ride_length” calculation to all_trips (trip duration in minutes, I prefer it without decimal)

```{r}
all_trips$ride_length <- as.integer(difftime(all_trips$ended_at,all_trips$started_at, units = "min"))
```


Double check the structure of the columns

```{r}
str(all_trips)
```


Remove unwanted data

The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Cyclistic or ride_length was negative. I will create a new version of the dataframe (v2) in which data with negative values are removed.

```{r}
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```


STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

Descriptive analysis on ride_length (all figures in minutes)

```{r}
summary(all_trips_v2$ride_length)
```


Then I compare members and casual users by calculating the min, max, mean and median of the ride length.

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```

Then I calculate the average ride time by each day for members vs casual users

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

Notice that the days of the week are out of order. This is how I fix that:

```{r}
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

The average ride time by each day for members vs. casual riders:

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

Then I analyze ridership data by type and weekday.

```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by user type and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sorts
```

It turns out that members tend to ride bikes in every week days while casual rider ride bike mostly on weekends. Here is the visualization of the number of rides by rider type and weekday:

```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

Another visualization for average trip duration, which shows that the average riding time of casual riders is higher than that of members.

```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS

Finally, I create a csv file that I can use to visualize in Excel, Tableau, Power Bi or PowerPoint.

```{r}
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/ASUS/Documents/GitHub/CS-Cyclistic/avg_ride_length.csv')
```
