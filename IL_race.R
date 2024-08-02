#prep
source("alkamel_functions.R")
library(tidyr)
library(patchwork)
#graph prep
hyper_cols <- c("Lamborghini" = "limegreen", "Peugeot" = "grey", "BMW" = "brown", "Alpine" = "blue", "Toyota" = "black", "Cadillac" = "darkblue", "Porsche" = "gold", "Ferrari" = "red", "Isotta Fraschini" = "orange")
GT3_cols <- c("darkgreen", "brown", "yellow", "red2", "lightblue", "deeppink", "black", "orange", "gold")


#data
IL_race <- import_WEC("Interlagos_24/23_Analysis_Race_Hour 6 (1).csv") 
IL_weather <- import_weather("Interlagos_24/26_Weather_Race_Hour 6.csv", "%m/%d/%Y %I:%M:%S %p", -3)

#preliminary join which will discover some weather measurements are missing (or at least it usually does)
IL_with_weather <- join_weather(IL_race, IL_weather, "2024-07-14")

#find them
times <- unique(filter(IL_with_weather, is.na(TRACK_TEMP))$approx_time)

#and now exclude them (or what the function actually does is take the previous minute's observation)
IL_with_weather <- join_weather(IL_race, IL_weather, "2024-07-14", times)

Spa_24 <- import_WEC("Spa_24/23_Analysis_Race_Hour 6 (1).csv") %>% 
  ungroup() %>%
  mutate(CLASS = factor(CLASS, levels = c("HYPERCAR", "LMGT3")), 
         S1_3 = S1_SECONDS + S3_SECONDS)

#weather
Spa_weather <- import_weather("Spa_24/26_Weather_Race_Hour 6.csv", "%m/%d/%Y %I:%M:%S %p", 2)

#preliminary join which will discover some weather measurements are missing (or at least it usually does)
Spa_with_weather <- join_weather(Spa_24, Spa_weather, "2024-05-11")

#find them
times <- unique(filter(Spa_with_weather, is.na(TRACK_TEMP))$approx_time)

#and now exclude them (or what the function actually does is take the previous minute's observation)
Spa_with_weather <- join_weather(Spa_24, Spa_weather, "2024-05-11", times) 

Spa_hypercar <- filter(Spa_24, CLASS == "HYPERCAR", S1_3 < 72) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(S1_3), 
         medlap = median(S1_3), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

#laptime histogram to find limits
ggplot2::ggplot(IL_race, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(85, 111) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

#this suggests max times around 95 seconds from Hypercar and 105 seconds for LMGT3

#class split
IL_hypercar <- filter(IL_with_weather, CLASS == "HYPERCAR", LAP_TIME < 95) %>% 
  drop_na(LAP_TIME) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

IL_GT3 <- filter(IL_with_weather, CLASS == "LMGT3", LAP_TIME < 105) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

IL_noBronzes <- (IL_GT3 %>% 
                   filter(!(DRIVER_NAME %in% bronze_drivers)) %>% 
                   mutate(meanlap = mean(as.numeric(LAP_TIME)), 
                          medlap = median(as.numeric(LAP_TIME)), 
                          medla2 = median(S2_SECONDS), 
                          speed = median(TOP_SPEED), na.rm = T))

#So our first question here is "where were Toyota fast?" 
IL_comped_H <- compare_sectors(IL_hypercar) %>% 
  group_by(MANUFACTURER) %>% 
  dplyr::mutate(meds1 = median(S1_SECONDS), 
                meds2 = median(S2_SECONDS), 
                meds3 = median(S3_SECONDS))

Spa_comped_H <- compare_sectors(Spa_hypercar) %>% 
  group_by(MANUFACTURER) %>% 
  dplyr::mutate(meds1 = median(S1_SECONDS), 
                meds2 = median(S2_SECONDS), 
                meds3 = median(S3_SECONDS))

s1_sp <- ggplot2::ggplot(IL_comped_H, aes(x = comp_S1, y = fct_reorder(MANUFACTURER, meds1), colour = MANUFACTURER)) +
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(-2, 2) + 
  labs(x = "Sector 1 compared to median sector 1 time", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

s2_sp <- ggplot2::ggplot(IL_comped_H, aes(x = comp_S2, y = fct_reorder(MANUFACTURER, meds2), colour = MANUFACTURER)) +
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(-2, 2) + 
  labs(x = "Sector 2 compared to median sector 1 time", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

s3_sp <- ggplot2::ggplot(IL_comped_H, aes(x = comp_S3, y = fct_reorder(MANUFACTURER, meds3), colour = MANUFACTURER)) +
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(-2, 2) + 
  labs(x = "Sector 3 compared to median sector 1 time", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

s1_sp + s2_sp + s3_sp

s1_spa <- ggplot2::ggplot(Spa_comped_H, aes(x = comp_S1, y = fct_reorder(MANUFACTURER, meds1), colour = MANUFACTURER)) +
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(-2, 2) + 
  labs(x = "Sector 1 compared to median sector 1 time", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

s2_spa <- ggplot2::ggplot(Spa_comped_H, aes(x = comp_S2, y = fct_reorder(MANUFACTURER, meds2), colour = MANUFACTURER)) +
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(-2, 2) + 
  labs(x = "Sector 2 compared to median sector 2 time", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

s3_spa <- ggplot2::ggplot(Spa_comped_H, aes(x = comp_S3, y = fct_reorder(MANUFACTURER, meds3), colour = MANUFACTURER)) +
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(-2, 2) + 
  labs(x = "Sector 3 compared to median sector 3 time", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

s1_spa + s2_spa + s3_spa

IL_vs_weather <- ggplot2::ggplot(filter(IL_comped_H, MANUFACTURER != "Isotta Fraschini"), aes(y = comp_S2, x = TRACK_TEMP, colour = MANUFACTURER)) + 
  geom_smooth(se = FALSE) +
  theme_classic() +
  labs(y = "Sector 2 compared to median sector 2 time", 
       x = "Track Temperature (Celsius)") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))