source('~/Dropbox/Tableau/Health Data/readGoogleSheet2.R')
library(lubridate)
library(xlsx)
library(dplyr)
setwd("~/Dropbox/Tableau/Health Data/")

activity_url<-'https://docs.google.com/spreadsheets/d/1tN9t7Npu120wuuSzBvCimSnEzxEpwFb0XkFb0JoiP-Q/pubhtml'
sleep_url<-'https://docs.google.com/spreadsheets/d/1N-4-Xv0t-3kTzCjq5zD4aqTe2S-y_x5BJAFU48ZpCqQ/pubhtml'

my_sleep<-tbl_df(readSpreadsheet(sleep_url))
my_sleep$FellAsleepAt <- mdy_hm(my_sleep$FellAsleepAt, tz = "EST")
my_sleep$AwokeAt <- mdy_hm(my_sleep$AwokeAt, tz = "EST")
column_names<-c(4,6,9,12)

my_sleep[column_names]<-sapply(my_sleep[column_names], as.numeric)
my_sleep<-mutate(my_sleep,ObservationDate=round_date(AwokeAt,"day"))
my_sleep<-group_by(my_sleep, ObservationDate)

sleep_summary<-my_sleep %>%
  ##group_by(Month = month(ObservationDate, label=TRUE),Weekday=wday(ObservationDate,label=TRUE,abbr = FALSE)) %>%
  ##summarise_each(funs(mean),matches("Seconds")) %>%
  group_by(Month = month(ObservationDate, label=TRUE)) %>%
  summarise(AvgTimeSleptInHours = mean(TotalTimeSleptInSeconds)/3600,
            ##AvgTimeInLightSleepHours = mean(TimeInLightSleepSeconds)/3600,
            AvgTimeInDeepSleepHours = mean(TimeInDeepSleepSeconds)/3600,
            AvgDeepSleepPercentage = AvgTimeInDeepSleepHours/AvgTimeSleptInHours*100) %>%
  arrange(Month)

my_activity<-tbl_df(readSpreadsheet(activity_url))
my_activity$DateOfActivity <- mdy(my_activity$DateOfActivity, tz = "EST")
column_names<-c(2:5,7,10)
my_activity[column_names]<-sapply(my_activity[column_names], as.numeric)
my_activity<-mutate(my_activity,ObservationDate=round_date(DateOfActivity,"day"))

activity_summary<-my_activity %>%
  group_by(Month = month(DateOfActivity,label=TRUE))%>%
  ##summarise_each(funs(mean),TotalSteps,DistanceCoveredMI,TotalCaloriesBurned)%>%
  summarise(AverageStepsPerDay = mean(TotalSteps),
            AverageDistanceCoveredMI = mean(DistanceCoveredMI),
            AverageCaloriesBurned=mean(TotalCaloriesBurned),
            AverageActiveTimeInSeconds = mean(TotalActiveTimeInSeconds))


up_summary<-merge(activity_summary,sleep_summary,by="Month") %>%
  write.xlsx(file = "up_report.xlsx",sheetName = "Summary",col.names = TRUE, row.names = FALSE)

up_details<-merge(my_activity,my_sleep,by="ObservationDate") %>%
  write.xlsx(file = "up_report.xlsx",sheetName = "Details",col.names = TRUE,append = TRUE,row.names = FALSE)





