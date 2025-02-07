# nolint start
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

data <- read.csv("../data/family_top_usage.csv")

data <- data %>%
        filter(team != "no_team")

base_colors <- rev(brewer.pal(6, "Greys"))
extended_palette <- colorRampPalette(base_colors)(10)

filtered_data <- data %>%
  group_by(team) %>%
  summarise(total_usage = sum(usage_count)) %>%
  arrange(desc(total_usage)) %>%
  slice_head(n = 6) %>%
  arrange(total_usage) %>%
  inner_join(data, by = "team")

custom_patterns <- rep(c("none", "stripe"), each = 5)

ggplot(filtered_data, aes(x = reorder(team, total_usage), y = usage_count, fill = family)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    # title = "Top 3 Used Families for Top 6 Verticals",
    title = "Top 3 famílias usadas por top 6 verticais",
    x = "Vertical",
    # y = "Instance Usage",
    y = "Número de instâncias",
    # fill = "Family"
    fill = "Família"
  ) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_manual(values = extended_palette) +
  theme_minimal() +
  theme(
    legend.spacing.y = unit(1.0, "cm"),
    plot.title = element_text(size = 25, margin = margin(t = 10, r = 0, b = 20, l = 0)),
    legend.title = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 10)),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
  ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  coord_flip()
# nolint end
