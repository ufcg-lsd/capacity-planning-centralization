# nolint start
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)

data <- read.csv("../data/mapping_by_time_10_year.csv")

data$month <- as.POSIXct(data$month)

data$year <- year(data$month)

yearly_data <- data %>%
  group_by(year) %>%
  summarise(avg_tagged_percentage = mean(tagged_percentage))

yearly_data <- yearly_data %>%
  arrange(year) %>%
  mutate(rolling_avg = rollmean(avg_tagged_percentage, k = 3, align = "right", fill = NA)) %>%
  mutate(window_label = ifelse(!is.na(rolling_avg), 
                               paste(year - 2, year, sep = "-"), NA))

yearly_data <- yearly_data %>%
  filter(!is.na(rolling_avg))

gg <- ggplot(yearly_data, aes(x = window_label, y = rolling_avg)) +
  geom_line(color = "black", group = 1) +
  geom_point(data = yearly_data %>% filter(window_label == "2021-2023"),
              aes(x = window_label, y = rolling_avg),
              color = "red", size = 10, shape = 1) +
  geom_text(data = yearly_data %>% filter(window_label == "2021-2023"),
              aes(x = window_label, y = rolling_avg, label = "78.1%"),
              vjust = -2, color = "red", size = 5) +
  labs(title = "3-Year Rolling Average of Tagged Percentage",
       x = "Time Window",
       y = "3-Year Rolling Average Tagged Percentage") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(gg)
# nolint end