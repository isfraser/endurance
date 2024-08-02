#helpers
source("alkamel_functions.R")
library(colorspace)
library(scales)

Monza_FP1 <- import_WEC("Monza/23_Analysis_Free Practice 1 (1).CSV", remove_pits = T)
Monza_FP2 <- import_WEC("Monza/23_Analysis_Free Practice 2 (1).CSV", remove_pits = T)

LMP2_FP1 <- Monza_FP1 %>%
  dplyr::filter(CLASS == "LMP2") 

LMP2_FP2 <- Monza_FP2 %>%
  dplyr::filter(CLASS == "LMP2")

LMP2_FP <- bind_rows(LMP2_FP1, LMP2_FP2) |>
  mutate(CAR = paste(TEAM, NUMBER, sep = " #")) 

ggplot2::ggplot(LMP2_FP, aes(x = ELAPSED, y = lap_seconds, colour = CAR)) + 
  geom_point() + 
  theme_classic() + 
  ylim(99, 115)

ggplot2::ggplot(LMP2_FP, aes(x = lap_seconds)) + 
  geom_histogram(bins = 100) + 
  theme_classic() + 
  xlim(99, 115)

LMP2_FP_processed <- LMP2_FP |>
  filter(LAP_TIME < 115)

ggplot2::ggplot(LMP2_FP_processed, aes(y = lap_seconds, x = DRIVER_NAME)) + 
  geom_violin() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 75))

LMP2_FP_summary <- LMP2_FP |>
  filter(LAP_TIME < 115) |>
  group_by(DRIVER_NAME) |>
  summarise(Average_lap = mean(LAP_TIME),
            Fastest_lap = min(LAP_TIME), 
            Number_laps = n())

LMP2_FP_top5 <- LMP2_FP |>
  arrange(LAP_TIME) |>
  group_by(DRIVER_NAME) |>
  slice(1:5)

#some more drivers
sub_drivers <- data.frame(Driver = c("Andrea CALDARELLI", "Mathias BECHE"), Rating = c("Platinum", "Gold"), Team = c("Prema", "Prema"))
Driver_ratings_LUT <- bind_rows(Driver_ratings_LUT, sub_drivers)
LMP2_FP_top5sumn <- LMP2_FP_top5 |>
  arrange(LAP_TIME) |>
  summarise(Average_lap = round(mean(LAP_TIME), 3), 
            Fastest_lap = min(LAP_TIME)) |>
  left_join(Driver_ratings_LUT, by = c("DRIVER_NAME" = "Driver"))

ratings_scale = c("Silver" = 'lightgrey', "Gold" = 'gold', "Platinum" = 'darkgrey')

ggplot(LMP2_FP_top5sumn, aes(x = Average_lap, y = reorder(DRIVER_NAME, Average_lap), fill = Rating)) + 
  geom_col() + 
  theme_classic() + 
  scale_x_continuous(limits = c(100, 103), oob = rescale_none) +  
  labs(x = "Average Lap Time (Best 5 laps)", 
       y = "Driver") +
  scale_fill_manual(values = ratings_scale) 
  