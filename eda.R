#install.packages("gridExtra")
library(ggplot2)
library(dplyr)
library(gridExtra)

data <- read.csv('./cleanData.csv')

# Male and female data
freq <- data %>%
  group_by(athlete.sex) %>%
  summarise(frequency = n()) %>%
  na.omit()

ggplot(freq, aes(x=athlete.sex, y=frequency)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Athlete Gender Comparison") + 
  xlab("Gender") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Male percent larger (3.29%)
total_athletes <- sum(freq$frequency)
male_perc <- freq$frequency[2] / total_athletes * 100 
female_perc <- freq$frequency[1] / total_athletes * 100
diff_perc <- (freq$frequency[2] - freq$frequency[1]) / total_athletes * 100

# female summary data
female <- data %>%
  filter(athlete.sex == "F")
summary(female)

# male summary data
male <- data %>%
  filter(athlete.sex == "M")
summary(male)

require(gridExtra)
plot1 <- ggplot(male, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  ggtitle("Male Distance Over Average Speed") + 
  xlab("Average Speed") +
  ylab("Distance") +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(female, aes(x= average_speed, y= distance, group = 1)) +
         geom_line(color="blue") +
  ggtitle("Female Distance Over Average Speed") + 
  xlab("Average Speed") +
  ylab("Distance") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2, ncol=2)

# Gain a deeper understanding of distance vs. speed plot for both genders
# Male speed under 500 and 15.
m_spd_avg <- mean(male$average_speed)
m_speed500 <- male %>%
  filter(average_speed < 500)

m_speed15 <- male %>%
  filter(average_speed < 15)

plot3 <- ggplot(m_speed500, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  ggtitle("Male Average Speed under 500") + 
  xlab("Average Speed") +
  ylab("Distance") +
  theme(plot.title = element_text(hjust = 0.5))

plot4 <- ggplot(m_speed15, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  geom_vline(aes(xintercept = m_spd_avg), color="red", linetype="dashed") +
  ggtitle("Male Average Speed under 10") + 
  xlab("Average Speed") +
  ylab("Distance") +
  theme(plot.title = element_text(hjust = 0.5))

# Female speed under 500 and 15.
f_spd_avg <- mean(female$average_speed)
f_speed500 <- female %>%
  filter(average_speed < 500)

f_speed15 <- female %>%
  filter(average_speed < 15)

plot5 <- ggplot(f_speed500, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  ggtitle("Male Average Speed under 500") + 
  xlab("Average Speed") +
  ylab("Distance") +
  theme(plot.title = element_text(hjust = 0.5))

plot6 <- ggplot(f_speed15, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  geom_vline(aes(xintercept = m_spd_avg), color="red", linetype="dashed") +
  ggtitle("Male Average Speed under 10") + 
  xlab("Average Speed") +
  ylab("Distance") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot3, plot4, plot5, plot6, nrow=2, ncol=2)

dev.off()
