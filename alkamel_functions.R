#libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(purrr)
library(lme4)
library(forcats)

#this attempts to parse the standard motorsports mm:ss.fff time and turn it into a lubridate duration object
sort_laptime <- function(times){
  time_list <- strsplit(times, ":")
  minutes <- as.numeric(sapply(time_list, "[[", 1))
  seconds <- as.numeric(sapply(time_list, "[[", 2))
  duration <- dminutes(minutes) + dseconds(seconds)
  return(duration)
}

#this attempts to split each cars race in to stints
find_stints <- function(df){ 
  df <- df %>% 
    mutate(stint = 1,
           stint_lap = 1)
  
  for(i in 2:nrow(df)){
    if(!is.na(df$PIT_TIME[i])) { # these are out laps
      df$stint[i] <- df$stint[i-1] + 1 # increment stint number when car leaves pits
      df$stint_lap[i] <- 1 # reset stint lap count
    } else if(df$NUMBER[i-1] != df$NUMBER[i]){ # different car
      df$stint[i] <- 1
      df$stint_lap[i] <- 1
    } else { # otherwise, same stint number and incrmement stint length
      df$stint[i] <- df$stint[i-1]
      df$stint_lap[i] <- df$stint_lap[i-1] + 1 
    }
  }
return(df)
}

import_WEC <- function(path, remove_pits = T){
  output <- read_delim(path, delim = ";", escape_double = FALSE, col_types = cols(DRIVER_NUMBER = col_character(), LAP_TIME = col_character(), 
                                                                                  S1 = col_character(), S2 = col_character(), S3 = col_character(), 
                                                                                  ELAPSED = col_character(), HOUR = col_time(format = "%H:%M:%S"), 
                                                                                  S1_LARGE = col_character(), ...30 = col_skip()), 
                       trim_ws = TRUE) %>%
    dplyr::mutate(LAP_TIME = sort_laptime(LAP_TIME),
                  lap_seconds =S1_SECONDS + S2_SECONDS + S3_SECONDS, 
                  CLASS = factor(CLASS, levels = c("HYPERCAR", "LMP2", "LMGTE Am", "LMGT3"))) 
  output <- find_stints(output)
    if(remove_pits == T) {
     output <- output %>%
    dplyr::filter(is.na(CROSSING_FINISH_LINE_IN_PIT), # in lap
                  is.na(PIT_TIME),                     # out lap
                  FLAG_AT_FL == "GF")                 # must be green running
    } 
  return(output)
}

import_IMSA <- function(path, remove_pits = T){
  output <- read_delim(path, delim = ";", escape_double = FALSE, col_types = cols(DRIVER_NUMBER = col_character(), LAP_TIME = col_character(), 
                                                                                  S1 = col_character(), S2 = col_character(), S3 = col_character(), 
                                                                                  ELAPSED = col_character(), HOUR = col_time(format = "%H:%M:%S"), 
                                                                                  S1_LARGE = col_character()), 
                       trim_ws = TRUE)[,1:26] %>%
    dplyr::mutate(LAP_TIME = sort_laptime(LAP_TIME), 
                  CLASS = if_else(CLASS == "GTDPRO", "GTD PRO", CLASS),
                  CLASS = factor(CLASS, levels = c("GTP", "LMP2", "LMP3", "GTD PRO", "GTD")))
  
  output <- find_stints(output) %>% 
    filter(if(remove_pits == T) {
      is.na(CROSSING_FINISH_LINE_IN_PIT) &            # in lap
      is.na(PIT_TIME) &                               # out lap
      FLAG_AT_FL == "GF"                             # must be green running
  } else {
    FLAG_AT_FL == "GF"                             # must be green running
    })
  
  return(output)
}

class_filters <- function(data, high, low){
  out <- data %>%
    dplyr::filter(LAP_TIME > seconds(low),
                  LAP_TIME < seconds(high))
  return(out)
}

# some colours
cols <- c("Absolute Racing" = "black", "Rinaldi #32" = "red1", "Rinaldi #33" = "red2", "Kessel Racing" = "yellow", "Iron Lynx" = "gold", "JMW Motorsport" = "tomato4", 
          "Oman Racing #69" = "azure3", "Proton #77" = "slategray4", "Iron Dames" = "deeppink", "Proton #93" = "darkgreen", "Oman Racing #95" = "orange")

manufacturers <- c("Toyota" = "black", 
                   "Lexus" = "black",
                   "Ferrari" = "red", 
                   "Cadillac" = "blue", 
                   "Corvette" = "yellow",
                   "Porsche" = "gold", 
                   "Peugeot" = "grey", 
                   "Alpine" = "lightblue", 
                   "BMW" = "brown", 
                   "Aston Martin" = "darkgreen", 
                   "Lamborghini" = "deeppink", 
                   "McLaren" = "orange")

compare_laps <- function(df){
  df <- df %>% 
    mutate(rounded_datetime = floor_date(TIME, "2 mins")) %>% # if the local time field is ever not this name that would need to be made dynamic, but this works for Daytona
    group_by(rounded_datetime) %>% 
    mutate(avg_lap_time = mean(LAP_TIME), 
           best_lap_time = min(LAP_TIME),
           lap_to_avg = LAP_TIME - avg_lap_time, 
           lap_to_best = LAP_TIME - best_lap_time) %>% 
    ungroup()
}

imsa_as_rally <- function(path){
  import_IMSA(path, remove_pits = F)
}

import_weather <- function(path, format, tz){
  read_delim(path, delim = ";", escape_double = FALSE, col_types = cols(TIME_UTC_SECONDS = col_number(), 
                                                                                                 ...10 = col_skip()), trim_ws = TRUE) %>% 
  dplyr::mutate(approx_time = floor_date(as_datetime(TIME_UTC_STR, format = format), "minute") + hours(tz))
}

join_weather <- function(laptimes, weather, date, no_temps = NULL, freedom_degrees = F){
  laptimes <- laptimes %>% 
    dplyr::mutate(TIME = parse_date_time(paste(date, HOUR), "Ymd HMS"), 
                  approx_time = floor_date(TIME, "minute"), 
                approx_time = if_else(approx_time %in% no_temps, approx_time - minutes(1), approx_time)) %>% 
  dplyr::left_join(select(weather, approx_time, TRACK_TEMP))
  if(freedom_degrees == T){
    output <- laptimes %>% 
    dplyr::mutate(Track_TempC = (TRACK_TEMP-32)*5/9)
  } else {
    output <- laptimes
  }
  return(output)
}

iterate_weather <- function(laptimes, weather, date, freedom_degrees = F){
  intermediate <- join_weather(laptimes, weather, date, freedom_degrees = freedom_degrees)
  no_temps <- unique(filter(intermediate, is.na(TRACK_TEMP))$approx_time)
  output <- join_weather(laptimes, weather, date, no_temps = no_temps, freedom_degrees = freedom_degrees)
  return(output)
}

compare_sectors <- function(df){
  out <- df %>% 
    ungroup() %>% 
    dplyr::mutate(med_S1 = median(S1_SECONDS), 
                  med_S2 = median(S2_SECONDS),
                  med_S3 = median(S3_SECONDS), 
                  comp_S1 = S1_SECONDS - med_S1, 
                  comp_S2 = S2_SECONDS - med_S2,
                  comp_S3 = S3_SECONDS - med_S3)
}

  #names of some bronzes to filter them out
bronze_drivers <- c("Ian JAMES", "Darren LEUNG", "Ahmad AL HARTHY", "Thomas FLOHR", "James COTTINGHAM", "Claudio SCHIAVONI", "Ryan HARDWICK", 
                    "Clément MATEU", "Tom VAN ROMPUY", "Hiroshi KOIZUMI", "Sarah BOVY", "Takeshi KIMURA", "Giorgio RODA", "Yasser SHAHIN", 
                    "Aliaksandr MALYKHIN", "Joshua CAYGILL", "Ben KEATING")  