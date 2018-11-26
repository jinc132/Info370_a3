library(dplyr)
data <- read.csv('./cleanData.csv', na.strings = c("", "NA"))

# Question 1 
# male
par(mfrow = c(4,4))
male_data <- data %>%
  filter(athlete.sex == "M") %>%
  na.omit()
  
male_movng_lm <- lm(moving_time ~ distance + average_speed + average_heartrate, data = male_data)
male_dist_lm <- lm(distance ~ average_speed + average_heartrate + moving_time, data = male_data)

summary(male_movng_lm)
summary(male_dist_lm)

plot(male_movng_lm)
plot(male_dist_lm)

#female
par(mfrow= c(4,4))
female_data <- data %>%
  filter(athlete.sex == "F") %>%
  na.omit()

fem_movng_lm <- lm(moving_time ~ distance + average_speed + average_heartrate, data = female_data)
fem_dist_lm <- lm(distance ~ average_speed + average_heartrate + moving_time, data = female_data)

summary(fem_movng_lm)
summary(fem_dist_lm)

plot(fem_movng_lm)
plot(fem_dist_lm)

