# Load the ggplot2 library
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Create the boxplot
ggplot(egg_counting1_summary, aes(x = diet, y = mean)) +
  geom_boxplot() +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(title = "Boxplot of Egg Counts by Diet",
       x = "Diet", y = "Mean Egg Count")




# Create the boxplot


egg_counting1_boxplot <- egg_counting1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_boxplot(stat = "identity",
               fill = "skyblue",
               colour = "orange",
               alpha = 0.6)
