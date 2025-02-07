# nolint start
library(ggplot2)
library(dplyr)

data <- read.csv("../data/verticals_in_years.csv")

data <- data %>%
  filter(team != "no_team") %>%
  group_by(year) %>%
  summarise(vertical_count = n()) %>%
  arrange(desc(vertical_count))

ggplot(data, aes(x = factor(year), y = vertical_count)) +
  geom_line(size = 1, group = 1) +
  geom_point(size = 3) +
  labs(
    # title = "Emergence of verticals (2013 - 2023)",
    # x = "Year",
    # y = "Vertical count"
    title = "Surgimento de verticais (2013 - 2023)",
    x = "Ano",
    y = "Número de verticais"
    ) + 
  theme_minimal() +
  theme(
    legend.spacing.y = unit(1.0, "cm"),
    plot.title = element_text(
      size = 25,
      margin = margin(t = 10, r = 0, b = 20, l = 0),
      hjust = 0.5
    ),
    legend.title = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 10)),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.margin = margin(t = 10, r = 50, b = 10, l = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# nolint end
