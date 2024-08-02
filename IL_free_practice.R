#prep
source("alkamel_functions.R")

#data
session_1 <- import_WEC("Interlagos_24/23_Analysis_Free Practice 1 (1).csv") %>% 
  filter(NUMBER != 78)
session_2 <- import_WEC("Interlagos_24/23_Analysis_Free Practice 2 (1).csv")

laptimes_list <- list(session_1, session_2)

interlagos_practice <- bind_rows(session_1, session_2)

ggplot2::ggplot(interlagos_practice, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(85, 111) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

#this suggests max times around 95 seconds form Hypercar and 107 seconds for LMGT3

#class split
IL_hypercar <- filter(interlagos_practice, CLASS == "HYPERCAR", LAP_TIME < 95 & TOP_SPEED > 200) %>% 
  drop_na(LAP_TIME) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(as.numeric(LAP_TIME)), 
         medlap = median(as.numeric(LAP_TIME)), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

IL_GT3 <- filter(interlagos_practice, CLASS == "LMGT3", LAP_TIME < 107 & TOP_SPEED > 200) %>% 
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

#graph prep
mean_times <- summarise(IL_hypercar, avg_lap = mean(LAP_TIME))
mean_GT3 <- as.data.frame(summarise(IL_GT3, avg_lap = mean(LAP_TIME)))

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

ggplot2::ggplot(IL_hypercar, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(200, 300) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(IL_GT3, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(95, 107) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(IL_noBronzes, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(95, 107) + 
  labs(x = "Lap Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(IL_noBronzes, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(210, 260) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))