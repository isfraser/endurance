#helpers
source("alkamel_functions.R")
library(colorspace)
library(stringr)

Sebring_WEC <- import_WEC("Sebring/23_Analysis_Race_Hour 8.CSV", remove_pits = F)

Ben_and_Sarah <- Sebring_WEC %>%
  dplyr::filter(NUMBER %in% c(33, 85)) %>%
  dplyr::mutate(ELAPSED = case_when(str_count(ELAPSED, "") == 8 ~ as.difftime(paste0("0:0", ELAPSED), format = "%H:%M:%S"),
                                    str_count(ELAPSED, "") == 9 ~ as.difftime(paste0("0:", ELAPSED), format = "%H:%M:%S"), 
                                    TRUE ~ as.difftime(ELAPSED, format = "%H:%M:%S"))) %>%
  dplyr::select(TEAM, ELAPSED, LAP_NUMBER, PIT_TIME) %>%
  tidyr::pivot_wider(names_from = TEAM, values_from = c(ELAPSED, PIT_TIME)) %>%
  dplyr::mutate(LEAD = `ELAPSED_Iron Dames` - `ELAPSED_Corvette Racing`, 
                `MOD_Iron Dames` = if_else(LAP_NUMBER >76, `ELAPSED_Iron Dames` - (235-60)/60, `ELAPSED_Iron Dames`),
                MOD_LEAD = `MOD_Iron Dames` - `ELAPSED_Corvette Racing`)

All_GTE <- Sebring_WEC %>%
  dplyr::filter(CLASS == "LMGTE Am") %>%
  dplyr::mutate(ELAPSED = case_when(str_count(ELAPSED, "") == 8 ~ as.difftime(paste0("0:0", ELAPSED), format = "%H:%M:%S"),
                                    str_count(ELAPSED, "") == 9 ~ as.difftime(paste0("0:", ELAPSED), format = "%H:%M:%S"), 
                                    TRUE ~ as.difftime(ELAPSED, format = "%H:%M:%S"))) %>%
  dplyr::select(TEAM, ELAPSED, LAP_NUMBER, PIT_TIME)

graph <- select(Ben_and_Sarah, LAP_NUMBER, LEAD, MOD_LEAD) %>%
  tidyr::pivot_longer(2:3)

ggplot2::ggplot(graph, aes(x = LAP_NUMBER, y = value, colour = name)) + geom_line() +
  theme_classic() + 
  scale_color_manual(labels = c("Actual", "Without Diffuser Loss"), values = c("red", "blue")) + 
  scale_y_continuous(breaks = seq(-1, 9, 1)) +
  labs(x = "Lap Number",
       y = "Corvette Lead (Minutes)",
       colour = "Scenario")

ggplot2::ggplot(All_GTE, aes(x = LAP_NUMBER, y = ELAPSED, group = TEAM, colour = TEAM)) + 
  theme_classic() + 
  geom_line() + 
  xlim(75, 100) + 
  ylim(165, 220)