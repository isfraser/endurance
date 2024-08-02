#libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(purrr)
library(lme4)

#this attempts to parse the standard motorsports mm:ss.fff time and turn it into a lubridate duration object
sort_laptime <- function(times){
  time_list <- strsplit(times, ":")
  minutes <- as.numeric(sapply(time_list, "[[", 1))
  seconds <- as.numeric(sapply(time_list, "[[", 2))
  duration <- dminutes(minutes) + dseconds(seconds)
  return(duration)
}

import_laps <- function(path){
  output <- read_delim(path, delim = ";", escape_double = FALSE, col_types = cols(DRIVER_NUMBER = col_character(), LAP_TIME = col_character(), 
                                                                                  S1 = col_character(), S2 = col_character(), S3 = col_character(), 
                                                                                  ELAPSED = col_character(), HOUR = col_time(format = "%H:%M:%S"), 
                                                                                  S1_LARGE = col_character(), ...30 = col_skip()), 
                                                  trim_ws = TRUE) %>%
    dplyr::mutate(LAP_TIME = sort_laptime(LAP_TIME),
                  lap_seconds =S1_SECONDS + S2_SECONDS + S3_SECONDS) %>%
    dplyr::filter(is.na(CROSSING_FINISH_LINE_IN_PIT), # in lap
                  is.na(PIT_TIME),                     # out lap
                  FLAG_AT_FL == "GF")                 # must be green running
                  
}

#Bahrain
FP1 <- import_laps(path = "~/R/endurance/Bahrain/FP1.csv") %>% dplyr::mutate(Session = "FP1")
FP2 <- import_laps(path = "~/R/endurance/Bahrain/FP2.csv") %>% dplyr::mutate(Session = "FP2")

Thurs_Practice <- bind_rows(FP1, FP2) 

#look at all the laps
ggplot2::ggplot(Thurs_Practice, aes(x = lap_seconds, colour = CLASS)) +
  theme_classic() + 
  geom_freqpoly() +
  xlim(105, 140) + 
  labs(x = "Laptime (seconds)", y = "Number of laps")
  
#split by class and filter to representative lap times

Class_laps <- Thurs_Practice %>% 
  split(.$CLASS) 

class_filters <- function(data, high, low){
  out <- data %>%
    dplyr::filter(lap_seconds > low,
                  lap_seconds < high)
  return(out)
}

All_laps <- bind_rows(representative_laps$HYPERCAR, representative_laps$LMP2, representative_laps$`LMGTE Pro`, representative_laps$`LMGTE Am`)
high <- list(HYPERCAR = 118, `LMGTE Am` = 135, `LMGTE Pro` = 126, LMP2 = 125) # weird order has to be the same as class_laps which is alphabetical
low <- list(HYPERCAR = 109, `LMGTE Am` = 118, `LMGTE Pro` = 116, LMP2 = 111)
representative_laps <- pmap(list(Class_laps, high, low), class_filters)

ggplot2::ggplot(All_laps, aes(x = Session, y = lap_seconds, color = CLASS)) + 
  geom_violin() + 
  theme_classic() + 
  labs(y = "Laptime (seconds)")

#regression analysis
laps_regression_HC <- lme4::lmer(lap_seconds ~ 1 + Session + HOUR + (1 | DRIVER_NAME) + (1 | TEAM), representative_laps$HYPERCAR)
laps_regression_Am <- lme4::lmer(lap_seconds ~ 1 + Session + HOUR + (1 | DRIVER_NAME) + (1 | TEAM), representative_laps$`LMGTE Am`)