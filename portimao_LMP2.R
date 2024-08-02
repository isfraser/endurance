#helpers
source("alkamel_functions.R")
library(colorspace)
library(stringr)
library(scales)

Portimao_WEC <- import_WEC("Portimao/23_Analysis_Race_Hour 6.CSV", remove_pits = T)
  
LMP2_raw <- Portimao_WEC %>%
  dplyr::filter(CLASS == "LMP2") %>%
  dplyr::mutate(ELAPSED = case_when(str_count(ELAPSED, "") == 8 ~ as.difftime(paste0("0:0", ELAPSED), format = "%H:%M:%S"),
                                    str_count(ELAPSED, "") == 9 ~ as.difftime(paste0("0:", ELAPSED), format = "%H:%M:%S"), 
                                    TRUE ~ as.difftime(ELAPSED, format = "%H:%M:%S")), 
                CAR = paste(TEAM, NUMBER, sep = " #")) %>%
  dplyr::select(CAR, DRIVER_NAME, FLAG_AT_FL, lap_seconds, ELAPSED, LAP_NUMBER, PIT_TIME, CROSSING_FINISH_LINE_IN_PIT) 

ggplot2::ggplot(LMP2_raw, aes(x = ELAPSED, y = lap_seconds, colour = CAR)) + 
  geom_point() + 
  theme_classic()

ggplot2::ggplot(LMP2_raw, aes(x = lap_seconds)) + 
  geom_histogram() + 
  theme_classic()

LMP2_racing <- LMP2_raw %>%
  dplyr::filter(lap_seconds < 102) %>%
  dplyr::mutate(stint = 1)
for(i in 2:nrow(LMP2_racing)){
  if(LMP2_racing$CAR[i] != LMP2_racing$CAR[i-1]){
    LMP2_racing$stint[i] <- 1
  } else if (LMP2_racing$DRIVER_NAME[i] != LMP2_racing$DRIVER_NAME[i-1]){
    LMP2_racing$stint[i] <- LMP2_racing$stint[i-1] + 1
  } else if (LMP2_racing$stint[i] < LMP2_racing$stint[i-1]){
    LMP2_racing$stint[i] <- LMP2_racing$stint[i - 1]
  }
                
}

Prema63 <- LMP2_racing %>%
  dplyr::filter(CAR == "Prema Racing #63")

United23 <- LMP2_racing %>%
  dplyr::filter(CAR == "United Autosports #23")

both <- bind_rows(Prema63, United23)

ggplot2::ggplot(Prema63, aes(x = LAP_NUMBER, y = lap_seconds, group = stint, colour = DRIVER_NAME)) + 
  geom_line() + 
  theme_classic() + 
  ylab("Lap Time") + 
  xlab("Lap Number") + 
  labs(colour = "Driver")
  
ggplot2::ggplot(United23, aes(x = ELAPSED, y = lap_seconds, group = stint, colour = DRIVER_NAME)) + 
  geom_line() + 
  theme_classic()

ggplot2::ggplot(LMP2_racing, aes(x = ELAPSED, y = lap_seconds, group = stint, colour = DRIVER_NAME)) + 
  geom_line() + 
  facet_wrap(~CAR) + 
  theme_classic()

regression <- lme4::lmer(lap_seconds ~ ELAPSED + (1 | DRIVER_NAME), data = LMP2_racing)
summary(regression)
lap_time_adjustment <- -0.0005709 # too lazy to remember which package coef() is in

LMP2_adjusted <- LMP2_racing %>%
  dplyr::mutate(adjusted_lap = lap_seconds - ELAPSED * lap_time_adjustment)

average_lt <- LMP2_adjusted %>%
  dplyr::group_by(LAP_NUMBER) %>%
  dplyr::summarise(avg_time = mean(lap_seconds), 
                   avg_adjusted = mean(adjusted_lap))

ggplot2::ggplot(average_lt, aes(x = LAP_NUMBER, y = avg_time)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(96, 99) + 
  labs(x = "Lap Number", 
       y = "Average Lap Time (seconds, LMP2)")

ggplot2::ggplot(average_lt, aes(x = LAP_NUMBER, y = avg_adjusted)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic() + 
  ylim(96, 99) + 
  labs(x = "Lap Number", 
       y = "Average Lap Time (seconds, LMP2)")

average_noagstart <- LMP2_adjusted %>%
  dplyr::filter(CAR %in% c("Vector Sport #10", "United Autosports #23", "United Autosports #22")) %>%
  dplyr::group_by(LAP_NUMBER) %>%
  dplyr::summarise(avg_time = mean(lap_seconds), 
                   avg_adjusted = mean(adjusted_lap))
  
ggplot2::ggplot(average_noagstart, aes(x = LAP_NUMBER, y = avg_time)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    theme_classic() + 
    ylim(95, 100) + 
    labs(x = "Lap Number", 
         y = "Average Lap Time (seconds, LMP2)")

drivers_mixedeffects <- ranef(regression)$DRIVER_NAME %>%
  dplyr::arrange(`(Intercept)`) %>%
  tibble::rownames_to_column() 

names(drivers_mixedeffects) <- c("Driver", "Driver Fixed Effect")
drivers <- drivers_mixedeffects$Driver

Driver_ratings_LUT <- data.frame(Driver = drivers, Rating = factor(c("Gold", "Platinum", "Gold", "Platinum", "Gold", "Platinum", "Gold", "Platinum", "Platinum", "Gold", "Gold", "Platinum", "Gold", "Gold", "Platinum", "Gold", "Gold", "Silver", "Platinum", "Silver", "Silver", "Gold", "Silver", "Silver", "Silver", "Gold", "Gold", "Silver", "Silver", "Gold", "Silver", "Silver", "Silver", "Silver", "Silver", "Silver"), 
                                                                   levels = c("Silver", "Gold", "Platinum")), Team = c("United Autosports", "United Autosports", "WRT", "United Autosports", "WRT", "WRT", "Prema", "Prema", "Inter Europol", "United Autosports", "Jota", "Prema", "Jota", "Inter Europol", "WRT", "Alpine", "Vector", "Prema", "Jota", "Jota", "Prema", "Alpine", "Prema", "United Autosports", "WRT", "Jota", "Alpine", "Vector", "United Autosports", "Alpine", 
                                                                                                                       "Alpine", "WRT", "Vector", "Inter Europol", "Alpine", "Jota"))
drivers_mixedeffects <- drivers_mixedeffects %>%
  dplyr::left_join(Driver_ratings_LUT) %>%
  dplyr::mutate(Driver = factor(Driver, levels = drivers))

drivers_avgadjusted <- LMP2_adjusted %>%
  dplyr::group_by(DRIVER_NAME) %>%
  dplyr::summarise(avg_lap = seconds(mean(adjusted_lap))) %>%
  dplyr::arrange(avg_lap) %>%
  dplyr::mutate(Driver = factor(DRIVER_NAME, levels = drivers)) 
                

ggplot2::ggplot(drivers_avgadjusted, aes(x = Driver, y = avg_lap)) + 
  geom_col() + 
  theme_classic() + 
  scale_y_continuous(limits = c(96, 99), oob = rescale_none) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y = "Average Lap Time")

ratings_scale = c("Silver" = 'lightgrey', "Gold" = 'gold', "Platinum" = 'darkgrey')

teams_scale = c("United Autosports" = 'darkblue', "Alpine" = 'lightblue', "Inter Europol" = "lightgreen", "WRT" = "red", "Prema" = "darkgreen", "Vector" = "brown", "Jota" = "lightgrey")
ggplot2::ggplot(drivers_mixedeffects, aes(x = Driver, y = `Driver Fixed Effect`, fill = Rating)) + 
  geom_col() + 
  theme_classic() + 
  scale_fill_manual(values = ratings_scale) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y = "Driver effect on lap time compared to average (seconds)")
  
ggplot2::ggplot(drivers_mixedeffects, aes(x = Driver, y = `Driver Fixed Effect`, fill = Team)) + 
  geom_col() + 
  theme_classic() + 
  scale_fill_manual(values = teams_scale) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y = "Driver effect on lap time compared to average (seconds)")
