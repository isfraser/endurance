source("alkamel_functions.R")
library(colorspace)
library(tidyverse)
library(lubridate)
library(lme4)

PLM_Race <- import_IMSA("PLM_23/23_Time Cards_Race.CSV", remove_pits = T) %>%
  dplyr::mutate(TIME = parse_date_time(paste("2023-10-14", HOUR), "Ymd HMS"), 
                approx_time = floor_date(TIME, "minute")) 


PLM_weather <- read_delim("PLM_23/26_Weather_Race.csv", 
                        delim = ";", escape_double = FALSE, col_types = cols(TIME_UTC_SECONDS = col_number(), 
                           ...10 = col_skip()), trim_ws = TRUE) %>% 
  dplyr::mutate(approx_time = floor_date(as_datetime(TIME_UTC_STR, format = "%d-%b-%y %H:%M:%S"), "minute") - hours(4)) # round the time off and also move to local track time to line up with the lap times
  
#before joining we need to fix the fact that some minutes don't have temps. These are
# 12:28
# 14:03
# 16:47
# 20:02
no_temps <- parse_date_time(paste("2023-10-14", c("12:28:00", "14:03:00", "16:47:00", "20:02:00"), sep = " "), "Ymd HMS")
PLM_Race_with_weather <- PLM_Race %>% 
  dplyr::mutate(approx_time = if_else(approx_time %in% no_temps, approx_time - minutes(1), approx_time)) %>% 
  dplyr::left_join(select(PLM_weather, approx_time, TRACK_TEMP)) %>% 
  dplyr::mutate(Track_TempC = (TRACK_TEMP-32)*5/9)

#first looks
ggplot2::ggplot(PLM_Race, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(70, 90) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

PLM_Processed <- class_filters(PLM_Race_with_weather, 90, 70) %>% 
  mutate(type = if_else(CLASS == "GTD PRO", "GTD", as.character(CLASS))) # if I only care about GT cars in general

PLM_byclass <- PLM_Processed %>%
  split(.$CLASS)

PLM_bytype <- PLM_Processed %>%
  split(.$type)
# look at track temp
ggplot2::ggplot(PLM_Race_with_weather, aes(x = Track_TempC, y = LAP_TIME, colour = CLASS)) + 
  geom_point() + 
  theme_classic() + 
  ylim(70, 90) + 
  labs(y = "Lap Time (seconds)", 
       x = "Track Temperature")

ggplot2::ggplot(PLM_byclass$GTP, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(70, 80) + 
  labs(y = "Lap Time (seconds)", 
       x = "Track Temperature (Farenheit)")

ggplot2::ggplot(PLM_byclass$LMP2, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(72.5, 85) + 
  labs(y = "Lap Time (seconds)", 
       x = "Track Temperature (Farenheit)")

ggplot2::ggplot(PLM_byclass$LMP3, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(77, 87) + 
  labs(y = "Lap Time (seconds)", 
       x = "Track Temperature (Farenheit)")

ggplot2::ggplot(PLM_byclass$`GTD PRO`, aes(x = TRACK_TEMP, y = LAP_TIME)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(80, 85) + 
  labs(y = "Lap Time (seconds)", 
       x = "Track Temperature (Farenheit)")

ggplot2::ggplot(PLM_byclass$GTD, aes(x = Track_TempC, y = LAP_TIME)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(80, 85) + 
  labs(y = "Lap Time (seconds)", 
       x = "Track Temperature")

order_drivers <- function(df){
  out <- df %>% 
    group_by(DRIVER_NAME) %>% 
    summarise(med_lap = median(as.numeric(LAP_TIME)), 
           min_lap = min(as.numeric(LAP_TIME)))  %>% 
    arrange(med_lap) 
  
out$rank <- 1:nrow(out)
    
    return(out)
}

GTDrank <- order_drivers(PLM_byclass$GTD)

ggplot2::ggplot(PLM_byclass$GTD, aes(y = DRIVER_NAME, x = LAP_TIME)) + 
  geom_violin() + 
  theme_classic() + 
  xlim(80, 90) + 
  scale_y_discrete(limits = rev(GTDrank$DRIVER_NAME)) + 
  labs(x = "Lap Time (seconds)", 
       y = element_blank())

ggplot2::ggplot(PLM_byclass$GTD, aes(y = MANUFACTURER, x = LAP_TIME)) + 
  geom_boxplot() + 
  theme_classic() + 
  xlim(80, 90) + 
  labs(x = "Lap Time (seconds)", 
       y = element_blank())

ggplot2::ggplot(filter(PLM_Processed, CLASS != "LMP2"), aes(y = fct_reorder(MANUFACTURER, LAP_TIME, .fun = min, .desc = TRUE), x = LAP_TIME)) + 
  geom_boxplot(notch = TRUE) + 
 # geom_jitter() + 
  theme_classic() + 
  xlim(70, 90) + 
  labs(x = "Lap Time (seconds)", 
       y = element_blank()) + 
  facet_wrap(~CLASS)

ggplot2::ggplot(filter(PLM_bytype$GTD), aes(y = fct_reorder(TEAM, MANUFACTURER), x = LAP_TIME, colour = MANUFACTURER)) + 
  geom_boxplot() + 
  # geom_jitter() + 
  theme_classic() + 
  xlim(80, 90) + 
  labs(x = "Lap Time (seconds)", 
       y = element_blank()) + 
  facet_grid(~CLASS)

ggplot2::ggplot(filter(PLM_bytype$LMP2), aes(y = fct_reorder(TEAM,  LAP_TIME, .fun = min, .desc = TRUE), x = LAP_TIME)) + 
  geom_boxplot(notch = TRUE) + 
  #geom_jitter() + 
  theme_classic() + 
  xlim(72.5, 85) + 
  labs(x = "Lap Time (seconds)", 
       y = element_blank()) + 
  facet_grid(~CLASS)

ggplot2::ggplot(PLM_Processed, aes(y = LAP_TIME, x = stint_lap)) + 
  geom_bin2d(bins = 25) + 
  # geom_jitter() + 
  theme_classic() + 
  ylim(70, 90) + 
  labs(y = "Lap Time (seconds)", 
       x = "Time since start of race") + 
  facet_wrap(~CLASS)

ggplot2::ggplot(filter(PLM_Processed, NUMBER == 60), aes(y = LAP_TIME, x = stint_lap)) + 
 # geom_bin2d(bins = 25) + 
  geom_point() + 
  theme_classic() + 
  ylim(70, 90) + 
  labs(y = "Lap Time (seconds)", 
       x = "Time since start of race") + 
  facet_wrap(~CLASS)