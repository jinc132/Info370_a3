library(dplyr)
library(corrplot)
data <- read.csv('./cleanData.csv', na.strings = c("", "NA"))

# Question 1 
# Remove outliers from the data for a more accurate model 
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm)
  
  y <- x
  
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
summary(male_data$average_speed)
# Begin removing outliers and create a male and female data frame
distance <- remove_outliers(data$distance)
moving_time <- remove_outliers(data$moving_time)
average_speed <- remove_outliers(data$average_speed)
max_speed <- remove_outliers(data$max_speed)
elapsed_time <- remove_outliers(data$elapsed_time)

clean_data <- data.frame(data$athlete.sex, data$athlete.country, distance, moving_time, average_speed, max_speed, elapsed_time) %>%
  na.omit()

male_data <- clean_data %>%
  filter(data.athlete.sex == "M")

female_data <- clean_data %>%
  filter(data.athlete.sex == "F")

# male model
par(mfrow = c(4,4))

male_movng_lm <- lm(moving_time ~ average_speed + max_speed + elapsed_time + distance, data = male_data)

summary(male_movng_lm)

plot(male_movng_lm, main = "Male Exertion Model:")

# female model
fem_movng_lm <- lm(moving_time ~ average_speed + max_speed + elapsed_time + distance, data = female_data)

summary(fem_movng_lm)

plot(fem_movng_lm, main = "Female Exertion Model:")

# Q2 
# Calculate the number of athletes in each country
country_corr <- clean_data %>%
  group_by(data.athlete.sex, data.athlete.country) %>%
  mutate(athletes.in.country = n()) %>%
  na.omit()

country_corr <- country_corr %>%
  select(athletes.in.country, distance, average_speed, moving_time, elapsed_time)

par(mfrow = c(1,1))
correlations <- cor(country_corr[3:7], use = "pairwise.complete.obs")
corrplot.mixed(correlations, lower.col = "black", number.cex = .7)

male_focus <- country_corr %>%
  filter(data.athlete.sex == "M")

# model male athletes in for each country.
par(mfrow = c(4, 4))
lm_male <- lm (athletes.in.country ~ distance + average_speed + moving_time + elapsed_time, data = male_focus)
summary(lm_male)
plot(lm_male, main = "Male Model:")

female_focus <- country_corr %>%
  filter(data.athlete.sex == "F")

# model male athletes in for each country.
lm_female <- lm (athletes.in.country ~ distance + average_speed + moving_time + elapsed_time, data = female_focus)
summary(lm_female)
plot(lm_female, main = "Female Model:")

