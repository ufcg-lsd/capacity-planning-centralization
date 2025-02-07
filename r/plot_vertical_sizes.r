# nolint start
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)

data <- read.csv("../data/team_size_2021_2023.csv")

data_mapped <- data %>% filter(team != "no_team")
data_unmapped <- data %>% filter(team == "no_team")

data_mapped <- data_mapped %>% mutate(category = "Mapped")
data_unmapped <- data_unmapped %>% mutate(category = "Unmapped", team = "No Vertical")

data_combined <- rbind(data_mapped, data_unmapped)

total_records <- sum(data_combined$record_count)

category_totals <- data_combined %>%
  group_by(category) %>%
  summarise(record_count = sum(record_count)) %>%
  mutate(percentage = record_count / total_records * 100)

data_combined$team <- factor(data_combined$team, 
                             levels = c(
                               data_combined %>% 
                                 filter(category == "Mapped") %>%
                                 arrange(record_count) %>%
                                 pull(team),
                               "No Vertical"
                             ))

num_teams <- nrow(data_combined)
custom_colors <- c(colorRampPalette(brewer.pal(13, "Paired"))(num_teams - 1), "grey")

ggplot(data_combined, aes(x = category, y = record_count, fill = team)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Mapped vs. Unmapped demand (With Vertical Sizes)",
       x = NULL,
       y = "Record Count") +
  geom_text(data = category_totals,
            aes(x = category, y = record_count + 4000000, label = paste0(round(percentage, 1), "%")),
            inherit.aes = FALSE,
            color = "black", size = 5) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  guides(fill = guide_legend(title = "Vertical")) +
  theme(axis.ticks.x = element_blank())

# nolint end