#helpers
source("alkamel_functions.R")
library(colorspace)

Daytona_Race <- import_IMSA("Daytona/23_Time Cards_Race.CSV")[,1:26] %>%
  dplyr::mutate(CLASS = if_else(CLASS == "GTDPRO", "GTD PRO", CLASS),
                CLASS = factor(CLASS, levels = c("GTP", "LMP2", "LMP3", "GTD PRO", "GTD")), 
                TIME = if_else(HOUR < hms("13:42:30"), parse_date_time(paste("2023-01-29", HOUR), "Ymd HMS"), parse_date_time(paste("2023-01-28", HOUR), "Ymd HMS")))

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

ggplot2::ggplot(Daytona_byclass$GTP, aes(x = TIME, y = LAP_TIME)) + 
  geom_bin2d(bins = 50) + 
  theme_classic() + 
  scale_fill_viridis_b(option = "B") + 
  ylim(95, 105) + 
  labs(x = "Time", y = "Lap Time (seconds)", fill = "Number of laps")

ggplot2::ggplot(Daytona_byclass$LMP2, aes(x = TIME, y = LAP_TIME)) + 
  geom_bin2d(bins = 50) + 
  theme_classic() + 
  scale_fill_viridis_b(option = "B") + 
  ylim(99, 110) + 
  labs(x = "Time", y = "Lap Time (seconds)", fill = "Number of laps") 

ggplot2::ggplot(Daytona_byclass$LMP3, aes(x = TIME, y = LAP_TIME)) + 
  geom_bin2d(bins = 40) +  
  geom_smooth(method = "lm") +
  theme_classic() + 
  scale_fill_viridis_b(option = "B") + 
  ylim(103, 115) + 
  labs(x = "Time", y = "Lap Time (seconds)", fill = "Number of laps") 

ggplot2::ggplot(Daytona_byclass$`GTD PRO`, aes(x = TIME, y = LAP_TIME)) + 
  geom_bin2d(bins = 40) + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  scale_fill_viridis_b(option = "B") + 
  ylim(105, 115) + 
  labs(x = "Time", y = "Lap Time (seconds)", fill = "Number of laps") 

ggplot2::ggplot(Daytona_byclass$GTD, aes(x = TIME, y = LAP_TIME)) + 
  geom_bin2d(bins = 40) + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  scale_fill_viridis_b(option = "B") + 
  ylim(105, 115) + 
  labs(x = "Time", y = "Lap Time (seconds)", fill = "Number of laps") 

ggplot2::ggplot(Daytona_byclass$GTP, aes(x = TIME, y = LAP_TIME)) + 
  geom_point(aes(colour = NUMBER)) + 
  theme_classic() + 
  #scale_fill_viridis_b(option = "B") + 
  ylim(95, 102) + 
  labs(x = "Time", y = "Lap Time (seconds)", fill = "Number of laps")

