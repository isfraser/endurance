source("alkamel_functions.R")

#data
#lap chart
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

#Bronzes
bronze_drivers <- unique(Spa_GT3$DRIVER_NAME)[c(1, 4, 7, 8, 14, 17, 20, 23, 29, 31, 34, 37, 40, 43, 46, 49)]

#exploratory histogram
ggplot2::ggplot(Spa_24, aes(x = S1_3, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(65, 85) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

#class split
Spa_hypercar <- filter(Spa_24, CLASS == "HYPERCAR", S1_3 < 72) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(S1_3), 
         medlap = median(S1_3), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

Spa_GT3 <- filter(Spa_24, CLASS == "LMGT3", S1_3 < 85) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(S1_3), 
         medlap = median(S1_3), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

Spa_hypercar_W <- filter(Spa_with_weather, CLASS == "HYPERCAR", S1_3 < 72) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(S1_3), 
         medlap = median(S1_3), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

Spa_GT3_W <- filter(Spa_with_weather, CLASS == "LMGT3", S1_3 < 85) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(S1_3), 
         medlap = median(S1_3), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

Spa_GT3_W_noBronzes <- Spa_GT3_W %>% 
  filter(!(DRIVER_NAME %in% bronze_drivers)) %>% 
  mutate(meanlap = mean(S1_3), 
         medlap = median(S1_3), 
         medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

#graph prep
mean_times <- summarise(Spa_hypercar, avg_lap = mean(S1_3))
mean_GT3 <- as.data.frame(summarise(Spa_GT3, avg_lap = mean(S1_3)))
Spa_top <- filter(Spa_hypercar, MANUFACTURER != "Isotta Fraschini")

mean_top <- summarise(Spa_top, avg_lap = mean(LAP_TIME)) %>% pull(avg_lap)
hyper_cols <- c(Lamborghini = "limegreen", Peugeot = "grey", BMW = "brown", Alpine = "blue", Toyota = "black", Cadillac = "darkblue", Porsche = "gold", Ferrari = "red", `Isotta Fraschini` = "orange")
GT3_cols <- c("darkgreen", "brown", "yellow", "red2", "lightblue", "deeppink", "black", "orange", "gold")

ggplot2::ggplot(Spa_top, aes(x = S1_3, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(67, 72) + 
  labs(x = "Combined Sectors 1&3 Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_top, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(250, 325) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_top, aes(x = S2_SECONDS, y = fct_reorder(MANUFACTURER, medla2), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) + 
  theme_classic() + 
  xlim(57, 65) + 
  labs(x = "Combined Sectors 1&3 Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  scale_fill_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"))

ggplot2::ggplot(Spa_GT3, aes(x = S1_3, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(75, 85) + 
  labs(x = "Combined Sectors 1&3 Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_GT3_W_noBronzes, aes(x = S1_3, y = fct_reorder(MANUFACTURER, medlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(75, 85) + 
  labs(x = "Combined Sectors 1&3 Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_GT3_W_noBronzes, aes(x = S2_SECONDS, y = fct_reorder(MANUFACTURER, medla2), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(64, 74) + 
  labs(x = "Combined Sectors 1&3 Time (seconds)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_hypercar_W, aes(y = S1_3, x = TRACK_TEMP)) + 
  geom_point(size = 1) + 
  geom_smooth(method = "lm") +
  facet_wrap(~MANUFACTURER) + 
  theme_classic() + 
  ylim(67.5, 72) + 
  labs(y = "Combined Sectors 1&3 Time (seconds)", 
       x = "Track Temperature (degrees Celsius)",
       colour = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_GT3_W_noBronzes, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic() + 
  xlim(230, 270) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(Spa_GT3_W, aes(y = S1_3, x = TRACK_TEMP)) + 
  geom_point(size = 1) + 
  geom_smooth(method = "lm") +
  facet_wrap(~MANUFACTURER) + 
  theme_classic() + 
  #ylim(67.5, 72) + 
  labs(y = "Combined Sectors 1&3 Time (seconds)", 
       x = "Track Temperature (degrees Celsius)",
       colour = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))