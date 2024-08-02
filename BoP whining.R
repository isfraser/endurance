library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)
library(forcats)
BovyFlohr_tidy <- read_csv("BovyFlohr.csv", col_types = cols(Year = col_character(), 
                                                               Bovy = col_character(), Flohr = col_character(), 
                                                               ...5 = col_skip())) %>% 
  mutate(Bovy = sort_laptime(Bovy), 
         Flohr = sort_laptime(Flohr), 
         Race = paste(Race, Year)) %>% 
  select(-Year) %>% 
  pivot_longer(2:3, names_to = "Driver", values_to = "Laptime")
  
ggplot(BovyFlohr, aes(x = Laptime, y = Race)) + 
  geom_point(aes(colour = Driver)) + 
  geom_line(aes(group = Race))

BovyFlohr_wide <- read_csv("BovyFlohr.csv", col_types = cols(Year = col_character(), 
                                                             Bovy = col_character(), Flohr = col_character(), 
                                                             ...5 = col_skip())) %>% 
  mutate(Bovy = sort_laptime(Bovy), 
         Flohr = sort_laptime(Flohr), 
         Race = paste(Race, Year), 
         diff = Bovy - Flohr) %>% 
  select(-Year) %>% 
  rownames_to_column()

ggplot(BovyFlohr_wide, aes(x = diff, y = forcats::fct_reorder(Race, as.numeric(rowname)))) + 
  geom_col(fill = "blue") + 
  theme_classic() + 
  geom_vline(xintercept = 0) + 
  labs(x = "Qualifying lap difference (Negative Bovy faster, Positive Flohr faster)",
       y = "Race")