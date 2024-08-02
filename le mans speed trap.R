source("alkamel_functions.R")

#data
session_1 <- import_WEC("Le Mans_24/23_Analysis_Free Practice 1 (1).csv")
session_2 <- import_WEC("Le Mans_24/23_Analysis_Free Practice 2 (1).csv")
session_3 <- import_WEC("Le Mans_24/23_Analysis_Free Practice 3.csv")
session_4 <- import_WEC("Le Mans_24/23_Analysis_Free Practice 4.csv")
quali <- import_WEC("Le Mans_24/23_Analysis_Qualifying Practice.csv")
hyperpole <- import_WEC("Le Mans_24/23_Analysis_Hyperpole.csv")

#weather 
weather_1 <- import_weather("Le Mans_24/26_Weather_Free Practice 1.csv", "%m/%d/%Y %I:%M:%S %p", 2)
weather_2 <- import_weather("Le Mans_24/26_Weather_Free Practice 2.csv", "%m/%d/%Y %I:%M:%S %p", 2)
weather_3 <- import_weather("Le Mans_24/26_Weather_Free Practice 3.csv", "%m/%d/%Y %I:%M:%S %p", 2)
weather_4 <- import_weather("Le Mans_24/26_Weather_Free Practice 4.csv", "%m/%d/%Y %I:%M:%S %p", 2)
weather_q <- import_weather("Le Mans_24/26_Weather_Qualifying Practice.csv", "%m/%d/%Y %I:%M:%S %p", 2)
weather_h <- import_weather("Le Mans_24/26_Weather_Hyperpole.csv", "%m/%d/%Y %I:%M:%S %p", 2)


laptimes_list <- list(session_1, session_2, session_3, session_4, quali, hyperpole)
weather_list <- list(weather_1, weather_2, weather_3, weather_4, weather_q, weather_h)
dates_list <- c("2024-06-12", "2024-06-12", "2024-06-13", "2024-06-13", "2024-06-12", "2024-06-13")

laps_with_weather <- bind_rows(pmap(list(laptimes_list, weather_list, dates_list), iterate_weather))
all_sess <- bind_rows(session_1, session_2, session_3, session_4, quali, hyperpole)


#class split weather
hypercar_w <- filter(laps_with_weather, CLASS == "HYPERCAR", LAP_TIME < 225)
 
GT3_w <- filter(laps_with_weather, CLASS == "LMGT3", LAP_TIME < 255) 

P2_w <- filter(laps_with_weather, CLASS == "LMP2", LAP_TIME < 240) 
 
#quick check for sane laptime limits
ggplot(hypercar, aes(x = LAP_TIME)) + geom_histogram(bins = 100) + xlim(200, 250) # suggests upper of 225
ggplot(P2, aes(x = LAP_TIME)) + geom_histogram(bins = 100) + xlim(210, 250) # suggests upper of 240
ggplot(GT3, aes(x = LAP_TIME)) + geom_histogram(bins = 100) + xlim(235, 270) # suggests upper of 255

#class split speed
hypercar <- filter(all_sess, CLASS == "HYPERCAR") %>% 
  group_by(MANUFACTURER) %>% 
  mutate(medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED, na.rm = T))

GT3 <- filter(all_sess, CLASS == "LMGT3", TOP_SPEED > 270) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T)

P2 <- filter(all_sess, CLASS == "LMP2") %>% 
  group_by(TEAM) %>% 
  mutate(medla2 = median(S2_SECONDS), 
         speed = median(TOP_SPEED), na.rm = T, 
         TEAM = ifelse(TEAM == "United Autosports USA", "United Autosports", TEAM))

ggplot2::ggplot(GT3, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(270, 295) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(P2, aes(x = TOP_SPEED, y = fct_reorder(TEAM, -speed), colour = TEAM)) + 
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(300, 330) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  #scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(hypercar, aes(x = TOP_SPEED, y = fct_reorder(MANUFACTURER, -speed), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1, show.legend = F) +
  theme_classic() + 
  xlim(300, 345) + 
  labs(x = "Speed at Speed Trap (kph)", 
       y = "Manufacturer") + 
  scale_colour_manual(values = hyper_cols) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linetype = "solid"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

#laptimes versus track temp
ggplot2::ggplot(hypercar_w, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  facet_wrap(~MANUFACTURER) + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  labs(x = "Track Temperature", 
       y = "Lap Time (seconds)") + 
  scale_colour_manual(values = hyper_cols) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(P2_w, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  facet_wrap(~TEAM) + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  labs(x = "Track Temperature", 
       y = "Lap Time (seconds)") + 
  scale_colour_manual(values = hyper_cols) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))

ggplot2::ggplot(GT3_w, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  facet_wrap(~MANUFACTURER) + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  labs(x = "Track Temperature", 
       y = "Lap Time (seconds)") + 
  scale_colour_manual(values = hyper_cols) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12))