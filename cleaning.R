library(dplyr)

# Do men tend to exercise more intensely (taking into account both
# distance and speed) than women?

#B1X1 = distance 
#B2X2 = speed 

#y = B0 + B1X + B2X
#Read raw data
raw_data = read.csv("strava_activity.csv")

#average_watts, kilohoules for rides

# Leave only the columns needed
clean_data = raw_data %>%
  dplyr::select(athlete.sex, achievement_count, distance, average_speed, average_heartrate, type,
                average_watts, kilojoules, max_speed, elapsed_time, moving_time, 
                athlete.city, athlete.country, athlete.state) #%>%
  #na.omit()

#write leaned data to new .csv file
write.csv(clean_data, file="cleanData.csv", row.names=FALSE )
