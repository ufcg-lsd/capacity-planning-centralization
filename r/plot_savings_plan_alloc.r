# nolint start
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)

demand <- read.csv("../data/c5/total_demand.csv")
allocation <- read.csv("../data/c5/total_purchases_savings_plan.csv")

demand$datetime <- as.POSIXct(demand$hour * 3600, origin = "2021-01-01")
allocation$datetime <- as.POSIXct(allocation$hour * 3600, origin = "2021-01-01")

demand_long <- demand %>%
  pivot_longer(cols = -c(hour, datetime), names_to = "instance_type", values_to = "demand")

demand_summary <- demand_long %>%
  mutate(month = floor_date(datetime, "month")) %>%
  group_by(month) %>%
  summarize(total_demand = sum(demand))

allocation_summary <- allocation %>%
  mutate(month = floor_date(datetime, "month")) %>%
  group_by(month) %>%
  summarize(total_reserves = sum(value_active))

ggplot() +
  geom_line(data = demand_summary, 
            aes(x = month, y = total_demand, color = "Instance Demand"), 
            size = 1) +
  geom_area(data = allocation_summary, 
            aes(x = month, y = total_reserves, fill = "Savings Plan Reserves"), 
            alpha = 0.4) +
  labs(
    title = "Demand vs. Optimal Savings Plan Allocation",
    subtitle = "C5 Family in {vertical_name} Vertical (2021-2023)",
    x = "Month",
    y = "Value",
    color = "Legend",
    fill = "Legend"
  ) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_color_manual(values = c("Instance Demand" = "black")) +
  scale_fill_manual(values = c("Savings Plan Reserves" = "red")) +
  theme_minimal() +
  theme(
    legend.spacing.y = unit(1.0, "cm"),
    plot.title = element_text(
      size = 25,
      margin = margin(t = 10, r = 0, b = 10, l = 0),
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 20,
      hjust = 0.5,
      face = "italic"
    ),
    legend.title = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 10)),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.margin = margin(t = 10, r = 50, b = 10, l = 10)
  )

# nolint end