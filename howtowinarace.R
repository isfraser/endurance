#helpers
source("alkamel_functions.R")
library(colorspace)
library(stringr)

Bahrain <- import_WEC("Bahrain_23/23_Analysis_Race_Hour 8 (1).CSV", remove_pits = T) # won by the Dames, second place is 777
Portimao <- import_WEC("Portimao/23_Analysis_Race_Hour 6.CSV", remove_pits = T) # won by Corvette 33
Spa <- import_WEC("Spa/23_Analysis_Race_Hour 6(1).CSV", remove_pits = T) # won by AF Corse 83
Fuji <- import_WEC("Fuji_23/23_Analysis_Race_Hour 6 (1).CSV", remove_pits = T) # won by AF Corse 54

Dames_diff <- function(df, dames = 85, rival){
  output <- df %>%
  dplyr::filter(NUMBER %in% c(dames, rival)) %>%
  dplyr::mutate(ELAPSED = case_when(str_count(ELAPSED, "") == 8 ~ as.difftime(paste0("0:0", ELAPSED), format = "%H:%M:%S"),
                                    str_count(ELAPSED, "") == 9 ~ as.difftime(paste0("0:", ELAPSED), format = "%H:%M:%S"), 
                                    TRUE ~ as.difftime(ELAPSED, format = "%H:%M:%S"))) %>%
  dplyr::select(TEAM, ELAPSED, LAP_NUMBER, PIT_TIME) %>%
  tidyr::pivot_wider(names_from = TEAM, values_from = c(ELAPSED, PIT_TIME))
  
return(output)  
}

Bahrain_diff <- Dames_diff(Bahrain, 85, 777) %>% 
  dplyr::mutate(Diff = `ELAPSED_Iron Dames` - `ELAPSED_D'Station Racing`,
                Race = "Bahrain")
Portimao_diff <- Dames_diff(Portimao, 85, 33) %>% 
  dplyr::mutate(Diff = `ELAPSED_Iron Dames` - `ELAPSED_Corvette Racing`, 
                Race = "Portimao")
Spa_diff <- Dames_diff(Spa, 85, 83) %>% 
  dplyr::mutate(Diff = `ELAPSED_Iron Dames` - `ELAPSED_Richard Mille AF Corse`, 
                Race = "Spa")
Fuji_diff <- Dames_diff(Fuji, 85, 54) %>% 
  dplyr::mutate(Diff = `ELAPSED_Iron Dames` - `ELAPSED_AF Corse`, 
                Race = "Fuji")

Blap_numbers <- unique(Bahrain_diff$LAP_NUMBER)
Plap_numbers <- c(unique(Portimao_diff$LAP_NUMBER), 205:231)
Slap_numbers <- c(unique(Spa_diff$LAP_NUMBER), 139:231)
Flap_numbers <- c(unique(Fuji_diff$LAP_NUMBER), 209:231)

all_laps <- Reduce(intersect, list(Blap_numbers, Plap_numbers, Slap_numbers, Flap_numbers))

combined_diffs <- bind_rows(Bahrain_diff, Portimao_diff, Spa_diff, Fuji_diff) %>% 
  filter(!is.na(Diff), 
         LAP_NUMBER %in% all_laps)

ggplot2::ggplot(combined_diffs, aes(x = LAP_NUMBER, y = Diff, colour = Race)) + geom_line() +
  theme_classic() + 
  labs(x = "Lap Number",
       y = "Iron Dames vs Best Car (minutes)")
       
B_All_GTE <- filter(Bahrain, CLASS == "LMGTE Am") %>% 
  dplyr::mutate(ELAPSED = case_when(str_count(ELAPSED, "") == 8 ~ as.difftime(paste0("0:0", ELAPSED), format = "%H:%M:%S"),
                                                                                   str_count(ELAPSED, "") == 9 ~ as.difftime(paste0("0:", ELAPSED), format = "%H:%M:%S"), 
                                                                                   TRUE ~ as.difftime(ELAPSED, format = "%H:%M:%S")))
ggplot2::ggplot(B_All_GTE, aes(x = LAP_NUMBER, y = ELAPSED, group = TEAM, colour = TEAM)) + 
  theme_classic() + 
  geom_line() + 
  xlim(70, 100) + 
  ylim(150, 200)
  theme(axis.labels.y = element_blank())