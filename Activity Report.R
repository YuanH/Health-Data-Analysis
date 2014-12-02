source('~/Dropbox/Tableau/Health Data/readGoogleSheet2.R')
library(lubridate)
activity_url<-'https://docs.google.com/spreadsheets/d/1tN9t7Npu120wuuSzBvCimSnEzxEpwFb0XkFb0JoiP-Q/pubhtml'
sleep_url<-'https://docs.google.com/spreadsheets/d/1N-4-Xv0t-3kTzCjq5zD4aqTe2S-y_x5BJAFU48ZpCqQ/pubhtml'

my_activity<-tbl_df(readSpreadsheet(activity_url))
my_activity$DateOfActivity <- mdy(my_activity$DateOfActivity, tz = "EST")
##my_activity$DateOfActivity<-as.Date(my_activity$DateOfActivity,"%m/%d/%Y")
column_names<-c(2:5,7,10)
my_activity[column_names]<-sapply(my_activity[column_names], as.numeric)

my_activity %>%
  group_by(Month = month(DateOfActivity,label=TRUE))%>%
##  group_by(months(DateOfActivity)) %>%
## summarise(AvgSteps = mean(TotalSteps),
##            AvgDistanceCoveredMI = mean(DistanceCoveredMI),
##            AvgCaloriesBurned = mean(TotalCaloriesBurned)) %>%
  summarise_each(funs(mean),TotalSteps,DistanceCoveredMI,TotalCaloriesBurned)%>%
  print
