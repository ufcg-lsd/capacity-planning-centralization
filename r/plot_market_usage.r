# nolint start
library(ggplot2)
library(readr)
library(scales)

data <- read_csv("../data/market_by_month_10_years.csv")

data$month <- as.Date(paste0(data$month, "-01"), format = "%Y-%m-%d")

market_labels <- c(
  "SavingsPlan (covered)" = "Savings Plan",
  "On-Demand" = "On-Demand",
  "Spot" = "Spot",
  "Reserved (benefits)" = "Reserved"
)

market_colors <- c(
  "SavingsPlan (covered)" = "#656565",
  "On-Demand" = "#070707",
  "Spot" = "#bfbfbf",
  "Reserved (benefits)" = "#3c3c3c"
)

ggplot(data, aes(x = month, y = total_instances, fill = market)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    # title = "Market usage (grouped by month) - 09.2013-02.2024",
    # x = "Date",
    # y = "Number of Instances",
    # fill = "Market"
    title = "Uso de mercados (09/2013-02/2024)",
    x = "Mês",
    y = "Número de instâncias",
    fill = "Mercado"
  ) +
  scale_fill_manual(labels = market_labels, values = market_colors) +
  scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
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
