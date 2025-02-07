# nolint start
library(ggplot2)
library(dplyr)

data <- read.csv("../data/rolling_weighted_avg.csv")

ggplot(data, aes(x = time_window, y = rolling_weighted_avg, fill = time_window == "2021-2023")) +
  geom_bar(stat = "identity", color = "#363636") +  # Bar plot
  scale_fill_manual(values = c("TRUE" = "#cf3d3d", "FALSE" = "#363636"), guide = "none") +
  geom_text(data = data %>% filter(time_window == "2021-2023"),
            aes(x = time_window, y = rolling_weighted_avg, label = "78.1%"),
            vjust = -1, color = "#cf3d3d", size = 6.5) +
  labs(
    # title = "3-Year Average of Mapped Percentage",
    # x = "Time Window",
    # y = "Mapped Percentage"
    title = "Média de porção mapeada em janelas de 3 anos",
    x = "Janela de tempo",
    y = "Porção mapeada"
    ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
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
