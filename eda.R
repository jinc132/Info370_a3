#install.packages("gridExtra")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gridExtra)

data <- read.csv('./cleanData.csv', na.strings = c("", "NA"))

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
  xlab("Average Speed (m/s)") +
  ylab("Distance (m)") +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(female, aes(x= average_speed, y= distance, group = 1)) +
         geom_line(color="blue") +
  ggtitle("Female Distance Over Average Speed") + 
  xlab("Average Speed (m/s)") +
  ylab("Distance (m)") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2, ncol=2)

# Gain a deeper understanding of distance vs. speed plot for both genders
# Male speed under 500 m/s and 15 m/s.
m_spd_avg <- mean(male$average_speed)
m_speed500 <- male %>%
  filter(average_speed < 500)

m_speed15 <- male %>%
  filter(average_speed < 15)

plot3 <- ggplot(m_speed500, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  ggtitle("Male Average Speed under 500 m/s") + 
  xlab("Average Speed (m/s)") +
  ylab("Distance (m)") +
  theme(plot.title = element_text(hjust = 0.5))

plot4 <- ggplot(m_speed15, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  geom_vline(aes(xintercept = m_spd_avg), color="red", linetype="dashed") +
  ggtitle("Male Average Speed under 15 m/s") + 
  xlab("Average Speed (m/s)") +
  ylab("Distance (m)") +
  theme(plot.title = element_text(hjust = 0.5))

# Female speed under 500 m/s and 15 m/s.
f_spd_avg <- mean(female$average_speed)
f_speed500 <- female %>%
  filter(average_speed < 500)

f_speed15 <- female %>%
  filter(average_speed < 15)

plot5 <- ggplot(f_speed500, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  ggtitle("Female Average Speed under 500 m/s") + 
  xlab("Average Speed (m/s)") +
  ylab("Distance (m)") +
  theme(plot.title = element_text(hjust = 0.5))

plot6 <- ggplot(f_speed15, aes(x= average_speed, y= distance, group = 1)) +
  geom_line(color="blue") +
  geom_vline(aes(xintercept = f_spd_avg), color="red", linetype="dashed") +
  ggtitle("Female Average Speed under 15 m/s") + 
  xlab("Average Speed (m/s)") +
  ylab("Distance (m)") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot3, plot4, plot5, plot6, nrow=2, ncol=2)

# Type of workout for both genders
type_freq <- data %>%
  group_by(athlete.sex, type) %>%
  summarise(type_counts = n())

ggplot(type_freq, aes(x=type, y=type_counts, fill=athlete.sex)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  ggtitle("Common Type of Exercise Between Genders") +
  xlab("Type of Exercise") +
  ylab("Frequency") + 
  guides(fill = guide_legend(title = "Gender")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 0.5))

# Question 2 
# Exploring the common countrywide exercise type 
country_type <- data %>%
  group_by(type, athlete.country) %>%
  summarise(type_counts = n()) %>%
  na.omit()

summary(country_type)

# Filter data based on the summarized data
country_type <- country_type %>%
  filter(type_counts > 10)

ggplot(country_type, aes(x=athlete.country, y=type_counts, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  ggtitle("Common Type of Exercise Across Countries") +
  xlab("Countries") +
  ylab("Frequency") + 
  guides(fill = guide_legend(title = "Exercise Type")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 0.5))

# Examining the exercising gender population ratio in each country
gender_country <- data %>%
  group_by(athlete.sex, athlete.country) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  na.omit()

summary(gender_country)

gender_country <- gender_country %>%
  filter(total > 10)

# Plot Gender vs. Countries
ggplot(gender_country, aes(x=athlete.country, y = ifelse(test = athlete.sex == "M", 
                                                         yes = -total, no = total), fill=athlete.sex)) +
  geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(-1500, 1500, 200), labels = abs(seq(-1500, 1500, 200))) +
  coord_flip() +
  labs(title="Gender Vs. Countries", y= "Frequency", x= "Countries") +
  guides(fill = guide_legend(title = "Gender")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2") 

dev.off()

