#prep
source("alkamel_functions.R")
library(tidyr)

#data
session_1 <- import_WEC("CotA_24/23_Analysis_Free Practice 1 (1).csv")
session_2 <- import_WEC("Cota_24/23_Analysis_Free Practice 2 (1).csv")
session_3 <- import_WEC("Cota_24/23_Analysis_Free Practice 3.csv")
bronze_drivers <- c(bronze_drivers, "Ben KEATING") # look who's back
laptimes_list <- list(session_1, session_2, session_3)

austin_practice <- bind_rows(session_1, session_2, session_3)

ggplot2::ggplot(austin_practice, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(110, 151) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

#this suggests max times around 125 seconds form Hypercar and 140 seconds for LMGT3

#class split
TX_hypercar <- filter(austin_practice, CLASS == "HYPERCAR", LAP_TIME < 125 & TOP_SPEED > 200) %>% # if you aren't doing at least 200kph through the speed trap you weren't trying
  drop_na(LAP_TIME) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

TX_GT3 <- filter(austin_practice, CLASS == "LMGT3", LAP_TIME < 140 & TOP_SPEED > 200) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

TX_noBronzes <- (TX_GT3 %>% 
                   filter(!(DRIVER_NAME %in% bronze_drivers)) %>% 
                   mutate(meanlap = mean(as.numeric(LAP_TIME)), 
                          medlap = median(as.numeric(LAP_TIME)), 
                          medla2 = median(S2_SECONDS), 
                          speed = median(TOP_SPEED), na.rm = T))

#graph prep
mean_times <- summarise(TX_hypercar, avg_lap = mean(LAP_TIME))
mean_GT3 <- as.data.frame(summarise(TX_GT3, avg_lap = mean(LAP_TIME)))

mean_top <- summarise(Spa_top, avg_lap = mean(LAP_TIME)) %>% pull(avg_lap)
hyper_cols <- c(Lamborghini = "limegreen", Peugeot = "grey", BMW = "brown", Alpine = "blue", Toyota = "black", Cadillac = "darkblue", Porsche = "gold", Ferrari = "red", `Isotta Fraschini` = "orange")
GT3_cols <- c("darkgreen", "brown", "yellow", "red2", "lightblue", "deeppink", "black", "orange", "gold")

ggplot2::ggplot(IL_hypercar, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(85, 95) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(TX_hypercar, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(250, 320) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(TX_hypercar, aes(x = stint_lap, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
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

ggplot2::ggplot(TX_noBronzes, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(125, 107) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(TX_noBronzes, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(125, 135) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(TX_noBronzes, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(240, 270) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))