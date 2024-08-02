source("alkamel_functions.R")

#data
prologue_1 <- import_WEC("23_Analysis_1st Test Session.csv")
prologue_2 <- import_WEC("23_Analysis_2nd Test Session.csv")
prologue_3 <- import_WEC("23_Analysis_3rd Test Session.csv")
prologue_4 <- import_WEC("23_Analysis_4th Test Session.csv")

prologue_all <- bind_rows(prologue_1, prologue_2, prologue_3, prologue_4)
#first looks
ggplot2::ggplot(prologue_all, aes(x = LAP_TIME, colour = CLASS)) + 
  geom_freqpoly(bins = 100, size = 1) + 
  theme_classic() + 
  xlim(100, 125) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps")

prologue_hypercar <- filter(prologue_all, CLASS == "HYPERCAR", LAP_TIME < 109) %>% 
  group_by(MANUFACTURER) 
  
prologue_GT3 <- filter(prologue_all, CLASS == "LMGT3", LAP_TIME < 123) %>% 
  group_by(MANUFACTURER) %>% 
  mutate(meanlap = mean(LAP_TIME))

mean_times <- summarise(prologue_hypercar, avg_lap = mean(LAP_TIME))
mean_GT3 <- as.data.frame(summarise(prologue_GT3, avg_lap = mean(LAP_TIME)))
prologue_top <- filter(prologue_hypercar, MANUFACTURER %in% c("BMW", "Cadillac", "Ferrari", "Peugeot", "Porsche", "Toyota"))

mean_top <- summarise(prologue_top, avg_lap = mean(LAP_TIME)) %>% pull(avg_lap)
mean_cols <- c("brown", "blue", "red", "grey", "gold", "black")

GT3_cols <- c("darkgreen", "brown", "yellow", "red2", "lightblue", "deeppink", "black", "orange", "gold")
ggplot2::ggplot(prologue_top, aes(x = LAP_TIME, colour = MANUFACTURER)) + 
  geom_freqpoly(bins = 30, size = 1) +
  theme_classic() + 
  geom_vline(xintercept = mean_top, colour = mean_cols) + 
  xlim(100, 109) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps") + 
  scale_colour_manual(values = manufacturers)

ggplot2::ggplot(prologue_GT3, aes(x = LAP_TIME, y = fct_reorder(MANUFACTURER, meanlap), colour = MANUFACTURER)) + 
  geom_boxplot(notch = T, size = 1) +
  theme_classic()  + 
  xlim(114, 125) + 
  labs(x = "Lap Time (seconds)", 
       y = "Number of Laps") + 
  scale_colour_manual(values = manufacturers) + 
  theme(panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    linetype = "solid"))

