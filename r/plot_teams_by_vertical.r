# nolint start
library(ggplot2)
library(dplyr)

data <- read.csv("../data/teams_by_vertical_2021_2023.csv")

data <- data %>%
  arrange(desc(team_count)) %>%
  filter(vertical != "Unmapped") %>%
  mutate(
    vertical = factor(vertical, levels = vertical),
    percentage = team_count / sum(team_count) * 100,
    cumulative_percentage = cumsum(percentage)
  )

ggplot(data, aes(x = vertical, y = team_count)) +
  geom_bar(stat = "identity", fill = "#939393") +
  geom_line(aes(y = cumulative_percentage * max(team_count) / 100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = cumulative_percentage * max(team_count) / 100), color = "red", size = 2) +
  scale_y_continuous(
    # name = "Team count",
    name = "Número de times",
    # sec.axis = sec_axis(~ . / max(data$team_count) * 100, name = "Cumulative Percentage")
    sec.axis = sec_axis(~ . / max(data$team_count) * 100, name = "Percentagem acumulada")
  ) +
  labs(
    # title = "Distribution of teams in verticals",
    # x = "Vertical"
    title = "Distribuição de times por vertical",
    x = "Vertical"
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
