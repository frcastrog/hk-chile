#--------------------------------HK Survey Data--------------------------------#
#-Author: Francisca Castro ----------------------------- Created: May 03, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 03, 2024-#

# 1) Load packages

pacman::p_load(readxl, ggplot2, tidyr, dplyr, scales)

# 2) Load data

hk_data_events <- read_excel("data/hk-data/hk-data-events.xlsx")

# 3) Plot evolution protests

#- Convert the 'date' column to Date class
hk_data_events$date <- as.Date(hk_data_events$date)

#- Pivot data to long format
hk_data_events_long <- hk_data_events %>%
  select(date, `Violent events`, `Non-violent events`) %>%
  pivot_longer(cols = c(`Violent events`, `Non-violent events`), names_to = "Type", values_to = "Events")

#- Calculate the percentage of each type of event for each date
hk_data_events_long <- hk_data_events_long %>%
  group_by(date) %>%
  mutate(Percentage = Events / sum(Events) * 100)

#- Plotting
hk_events_plot <- ggplot(hk_data_events_long, aes(x = date, y = Percentage, fill = Type)) +
  geom_area(position = 'stack', alpha = 0.8) +
  scale_fill_manual(values = c(`Violent events` = "black", `Non-violent events` = "grey")) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%Y-%m-%d",
    limits = as.Date(c("2019-06-01", "2020-01-31")),  # Exact data range limits
    expand = c(0, 0),  # Ensure no expansion
    breaks = seq(as.Date("2019-06-01"), as.Date("2020-01-31"), by = "1 week")) +
  labs(x = "Date", y = "Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
    legend.position = "bottom",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Adjust plot margins if necessary
    axis.title.x = element_text(margin = margin(t = 20, b = 10))  # Adjust top and bottom margin of the x-axis title
  )

ggsave("outputs/hk_events_plot.png", plot = hk_events_plot, dpi = 600, width = 7, height = 5)
