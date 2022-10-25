#libraries
library(readr)
library(tidyverse)
library(zoo)
library(plotly)

#raw data
X23_Analysis_Port <- read_delim("C:/Users/isfra/Downloads/23_Analysis_Race.CSV", 
                                       delim = ";", escape_double = FALSE, col_types = cols(NUMBER = col_character(), 
                                                                                                      DRIVER_NUMBER = col_character(), 
                                                                                                      ELAPSED = col_double(), HOUR = col_time(format = "%H:%M:%S"), 
                                                                                                      S1_LARGE = col_character(), S2_LARGE = col_character(), 
                                                                                                      S3_LARGE = col_character(), ELAPSED = col_character()),
                                                                                                      trim_ws = TRUE)
X23_Analysis_Spa <- read_delim("C:/Users/isfra/Downloads/23_Analysis_Race(1).CSV", 
                                delim = ";", escape_double = FALSE, col_types = cols(NUMBER = col_character(), 
                                                                                     DRIVER_NUMBER = col_character(), 
                                                                                     ELAPSED = col_double(), HOUR = col_time(format = "%H:%M:%S"), 
                                                                                     S1_LARGE = col_character(), S2_LARGE = col_character(), 
                                                                                     S3_LARGE = col_character(), ELAPSED = col_character()),
                                trim_ws = TRUE)

#split lap chart by class
Portimao_byclass <- X23_Analysis_Race %>%
  split(.$CLASS)

Spa_byclass <- X23_Analysis_Spa %>%
  split(.$CLASS)

chart_processing <- function(data){ 
  data <- data %>%
    dplyr::mutate(total_lap_seconds = S1_SECONDS + S2_SECONDS + S3_SECONDS) %>%
    dplyr::filter(FLAG_AT_FL == "GF", 
                  is.na(PIT_TIME))  %>% #green laps only, no pitstops
    dplyr::mutate(Start_time = HOUR - total_lap_seconds) # at what time did this lap start? (HOUR is when it ended)
}

Portimao_processed <- purrr::map(Portimao_byclass, chart_processing)
Spa_processed <- purrr::map(Spa_byclass, chart_processing)

#look at histogram of lap times
Spa_raw_GTE <- Spa_processed[["LMGTE"]]

#method: compare the lap to all the other laps that happened in that class at the same time
compare_laps <- function(data){
  data <- data %>%
    dplyr::group_by(DRIVER_NAME) %>%
    dplyr::mutate(Average_Lap = 0,
                  Stint_Length = LAP_NUMBER - min(LAP_NUMBER) + 1) %>%
    dplyr::ungroup()
  
  for(i in 1:nrow(data)){
    compare <- data %>%
      dplyr::filter(HOUR <= HOUR[i] & HOUR >= Start_time[i]) %>%
      dplyr::summarise(mean(total_lap_seconds))
    data$Average_Lap[i] <- as.numeric(compare)
  }
  output <- data %>%
    dplyr::mutate(Performance_index = 100 * total_lap_seconds / Average_Lap)
}

Portimao_compared <- purrr::map(Portimao_processed, compare_laps)
Spa_compared <- purrr::map(Spa_processed, compare_laps)

Port_GTE <- Portimao_compared[["LMGTE"]]
Port_LMP2 <- Portimao_compared[["LMP2"]]
Port_LMP3 <- Portimao_compared[["LMP3"]]

Spa_GTE <- Spa_compared[["LMGTE"]]

ggplot2::ggplot(Port_GTE, aes(x = Stint_Length, y = Performance_index)) + 
  geom_line(aes(colour = DRIVER_NAME)) + 
  theme_classic() + 
  ylim(60, 120) + 
  xlim(0, 60)

Port_GTE <- Port_GTE %>% 
  dplyr::mutate(TEAM = dplyr::case_when(NUMBER == 83 ~ "Iron Dames",
                                        NUMBER == 69 ~ "Oman Racing #69",
                                        NUMBER == 95 ~ "Oman Racing #95",
                                        NUMBER == 77 ~ "Proton #77",
                                        NUMBER == 93 ~ "Proton #93",
                                        NUMBER == 32 ~ "Rinaldi #32",
                                        NUMBER == 33 ~ "Rinaldi #33",
                                        TRUE ~ TEAM)) 

PGTE_Index <- Port_GTE %>%
  dplyr::group_by(DRIVER_NAME, TEAM) %>% 
  dplyr::summarise(Performance_Index = mean(Performance_index)) %>%
  dplyr::arrange(Performance_Index)

Spa_GTE <- Spa_GTE %>% 
  dplyr::mutate(TEAM = dplyr::case_when(NUMBER == 83 ~ "Iron Dames",
                                        NUMBER == 69 ~ "Oman Racing #69",
                                        NUMBER == 95 ~ "Oman Racing #95",
                                        NUMBER == 77 ~ "Proton #77",
                                        NUMBER == 93 ~ "Proton #93",
                                        NUMBER == 32 ~ "Rinaldi #32",
                                        NUMBER == 33 ~ "Rinaldi #33",

ggplot(Spa_GTE, aes(x = total_lap_seconds, fill = TEAM)) + geom_histogram(bins = 60) + scale_fill_manual(values = cols)                                        TRUE ~ TEAM)) 
SGTE_index <- Spa_GTE
  dplyr::group_by(DRIVER_NAME, TEAM) %>% 
  dplyr::summarise(Performance_Index = mean(Performance_index)) %>%
  dplyr::arrange(Performance_Index)


Porder <- PGTE_index$DRIVER_NAME
Sorder <- SGTE_index$DRIVER_NAME 

cols <- c("Absolute Racing" = "black", "Rinaldi #32" = "red1", "Rinaldi #33" = "red2", "Kessel Racing" = "yellow", "Iron Lynx" = "gold", "JMW Motorsport" = "tomato4", 
          "Oman Racing #69" = "azure3", "Proton #77" = "slategray4", "Iron Dames" = "deeppink", "Proton #93" = "darkgreen", "Oman Racing #95" = "orange")
PGTE_index <- PGTE_index %>%
  dplyr::mutate(DRIVER_NAME = factor(DRIVER_NAME, levels = Porder))

SGTE_index <- SGTE_index %>%
  dplyr::mutate(DRIVER_NAME = factor(DRIVER_NAME, levels = Sorder))

ggplot(SGTE_index, aes(x = DRIVER_NAME, y = Performance_Index, fill = factor(TEAM))) + 
  geom_col() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = cols) + 
  labs(fill = "Team", x = "Driver", y = "Average Laptime compared to simultaneous laps")

ggplot(filter(GTE, DRIVER_NAME %in% c("Doriane PIN", "Marco SORENSEN", "Fabrizio CRESTANI", "Ahmad AL HARTHY", "Sarah BOVY")), aes(x = Stint_Length, y = Performance_index, colour = DRIVER_NAME)) + 
  geom_line() + 
  theme_classic() + 
  xlim(0, 25)