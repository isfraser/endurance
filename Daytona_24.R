library(tidyverse)
source("alkamel_functions.R")

Daytona_Race <- import_IMSA("Daytona_24/23_Time Cards_Race.CSV")[,1:26] %>%
  dplyr::mutate(CLASS = if_else(CLASS == "GTDPRO", "GTD PRO", CLASS),
                CLASS = factor(CLASS, levels = c("GTP", "LMP2", "LMP3", "GTD PRO", "GTD")), 
                TIME = if_else(HOUR < hms("13:42:30"), parse_date_time(paste("2024-01-28", HOUR), "Ymd HMS"), parse_date_time(paste("2024-01-27", HOUR), "Ymd HMS")))

#first looks
ggplot2::ggplot(Daytona_Race, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(90, 125) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

Daytona_Processed <- class_filters(Daytona_Race, 120, 95) 

Daytona_byclass <- Daytona_Processed %>%
  split(.$CLASS)

ggplot2::ggplot(filter(Daytona_byclass$GTD, TEAM == "Iron Dames"), aes(x = TIME, y = LAP_TIME, colour = DRIVER_NAME, group = TEAM)) + 
  geom_line() + 
  geom_smooth() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time (seconds)", 
       colour = "Driver")

GTD_compared <- compare_laps(Daytona_byclass$GTD)
All_GT_compared <- compare_laps(bind_rows(Daytona_byclass$GTD, Daytona_byclass$`GTD PRO`))

ggplot2::ggplot(filter(GTD_compared, TEAM == "Iron Dames"), aes(x = TIME, y = lap_to_avg, colour = DRIVER_NAME, group = TEAM)) + 
  geom_line() + 
  geom_smooth() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time (seconds)", 
       colour = "Driver")
ggplot2::ggplot(filter(GTD_compared, TEAM == "Iron Dames"), aes(x = TIME, y = lap_to_avg, colour = DRIVER_NAME, group = TEAM)) + 
  geom_line() + 
  geom_smooth() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time (seconds)", 
       colour = "Driver")

best_vs_avg_comparison <- select(All_GT_compared, TIME, avg_lap_time, best_lap_time, rounded_datetime) %>% 
  distinct() %>% 
  pivot_longer(cols = 2:3, names_to = "metric", values_to = "laptime")

size_of_bins <- count(All_GT_compared, rounded_datetime)
bad_bins <- size_of_bins$rounded_datetime[size_of_bins$n < 20]

All_GT_compared <- All_GT_compared %>% 
  filter(!(rounded_datetime %in% bad_bins))

best_vs_avg_comparison_filtered <- select(All_GT_compared, TIME, avg_lap_time, best_lap_time, rounded_datetime) %>% 
  distinct() %>% 
  pivot_longer(cols = 2:3, names_to = "metric", values_to = "laptime")

ggplot2::ggplot(best_vs_avg_comparison_filtered, aes(x = TIME, y = laptime, group = metric, colour = metric)) + 
  geom_line() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time (seconds)", 
       colour = "Metric") + 
  scale_color_manual(values = c("avg_lap_time" = "blue", "best_lap_time" = "red"), labels = c("Average lap time", "Best lap time"))

ggplot2::ggplot(filter(All_GT_compared, TEAM == "Iron Dames"), aes(x = TIME, y = avg_lap_time, group = TEAM)) + 
  geom_line() + 
  geom_smooth() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time (seconds)", 
       colour = "Driver")
ggplot2::ggplot(filter(All_GT_compared, TEAM == "Iron Dames"), aes(x = TIME, y = best_lap_time, group = TEAM)) + 
  geom_line() + 
  geom_smooth() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time (seconds)", 
       colour = "Driver")

ggplot2::ggplot(filter(All_GT_compared, TEAM == "Iron Dames"), aes(x = TIME, y = lap_to_best, group = TEAM, colour = DRIVER_NAME)) + 
  geom_line() + 
  geom_smooth() + 
  theme_classic() + 
  labs(x = "Day and Time", 
       y = "Lap Time compared to best laptime in those two minutes (seconds)", 
       colour = "Driver")

compare_GT_cars <- All_GT_compared %>% 
  group_by(MANUFACTURER) %>% 
  summarise(mean_to_best = mean(lap_to_best))

compare_GT_drivers <- All_GT_compared %>% 
  group_by(DRIVER_NAME) %>% 
  summarise(mean_to_best = mean(lap_to_best))

ggplot(compare_GT_cars, aes(x = fct_reorder(MANUFACTURER, mean_to_best), y = mean_to_best)) + 
  geom_col(fill = "blue") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, margin = margin(t = 5))) + 
  labs(x = "Manufacturer", 
       y = "Mean lap time compared to 2-minute block best")

ggplot(compare_GT_drivers, aes(y = fct_reorder(DRIVER_NAME, mean_to_best), x = mean_to_best)) + 
  geom_col(fill = "blue") + 
  theme_classic()  + 
  labs(y = "Driver", 
       x = "Mean lap time compared to 2-minute block best") 