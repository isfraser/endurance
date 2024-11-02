#prep
source("alkamel_functions.R")
library(tidyr)

#data
session_1 <- import_WEC("Fuji_24/23_Analysis_Free Practice 1 (1).csv")
session_2 <- import_WEC("Fuji_24/23_Analysis_Free Practice 2 (1).csv")
#session_3 <- import_WEC("Cota_24/23_Analysis_Free Practice 3.csv")
#bronze_drivers <- c(bronze_drivers, "Ben KEATING") # look who's back
laptimes_list <- list(session_1, session_2)

fuji_practice <- bind_rows(session_1, session_2)

#explotatory
ggplot2::ggplot(fuji_practice, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(85, 120) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

ggplot2::ggplot(fuji_practice, aes(x = TOP_SPEED, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(200, 350) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")
#this suggests max times around 100 seconds form Hypercar and 115 seconds for LMGT3

#class split
JP_hypercar <- filter(fuji_practice, CLASS == "HYPERCAR", LAP_TIME < 100 & TOP_SPEED > 250) %>% # if you aren't doing at least 250kph through the speed trap you weren't trying
  drop_na(LAP_TIME) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

JP_GT3 <- filter(fuji_practice, CLASS == "LMGT3", LAP_TIME < 115 & TOP_SPEED > 250) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

JP_noBronzes <- (JP_GT3 %>% 
                   filter(!(DRIVER_NAME %in% bronze_drivers)) %>% 
                   mutate(meanlap = mean(as.numeric(LAP_TIME)), 
                          medlap = median(as.numeric(LAP_TIME)), 
                          medla2 = median(S2_SECONDS), 
                          speed = median(TOP_SPEED), na.rm = T))

#graph prep
mean_times <- summarise(JP_hypercar, avg_lap = mean(LAP_TIME))
mean_GT3 <- as.data.frame(summarise(JP_GT3, avg_lap = mean(LAP_TIME)))
mean_noBronze <- as.data.frame(summarise(JP_noBronzes, avg_lap = mean(LAP_TIME)))

#mean_top <- summarise(Spa_top, avg_lap = mean(LAP_TIME)) %>% pull(avg_lap)
hyper_cols <- c(Lamborghini = "limegreen", Peugeot = "grey", BMW = "brown", Alpine = "blue", Toyota = "black", Cadillac = "darkblue", Porsche = "gold", Ferrari = "red", `Isotta Fraschini` = "orange")
GT3_cols <- c("darkgreen", "brown", "yellow", "red2", "lightblue", "deeppink", "black", "orange", "gold")

ggplot2::ggplot(JP_hypercar, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(89, 97) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14))

ggplot2::ggplot(JP_hypercar, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(275, 325) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14))

ggplot2::ggplot(JP_hypercar, aes(x = stint_lap, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(110, 125) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(JP_noBronzes, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(100, 107) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14))


ggplot2::ggplot(JP_noBronzes, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(250, 280) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14))