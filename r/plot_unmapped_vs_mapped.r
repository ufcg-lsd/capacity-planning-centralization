# nolint start
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(scales)

data <- read_csv("../data/2021-2023-total_demand.csv")

data$start_date <- as.POSIXct(data$start_date, format = "%Y-%m-%d %H:%M:%S")

data <- data %>% mutate(week = floor_date(start_date, unit = "week"))

top_teams <- data %>%
  group_by(team) %>%
  summarize(total_demand = sum(num_instances), .groups = "drop") %>%
  arrange(desc(total_demand)) %>%
  slice_head(n = 4) %>%
  pull(team)

data_filtered <- data %>% filter(team %in% top_teams)

data_grouped <- data_filtered %>%
  group_by(team, week) %>%
  summarize(demand = sum(num_instances), .groups = "drop")

data_grouped <- data_grouped %>%
  mutate(
    fill_color = case_when(
      team == "no_team" ~ "red",
      TRUE ~ "black"
    ),
    line_color = case_when(
      team == "no_team" ~ "red",
      TRUE ~ "black"
    )
  )

# mapeia os nomes originais das verticais para rótulos menores (para facets)
# "Vertical 1": como está no dataset; "V1": rótulo no plot
facet_labels <- c(
  "Vertical 1" = "V1",
  "Vertical 2" = "V2",
)

ggplot(data_grouped, aes(x = week, y = demand)) +
  geom_area(aes(fill = fill_color), alpha = 0.4, show.legend = FALSE) +
  geom_line(aes(color = line_color), size = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  facet_wrap(. ~ team, labeller = labeller(team = facet_labels)) +
  labs(
    # title = "Top 3 Vertical vs. Unmapped Demands",
    # x = "Week",
    # y = "Instance Count",
    title = "Top 3 Verticais vs. Demanda não mapeada",
    x = "Semana",
    y = "Número de instâncias",
  ) +
  scale_y_continuous(labels = label_number(scale_cut = cut_si("M"))) +
  theme_bw() +
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