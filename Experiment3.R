library(tidyverse)
library(readxl)
library(kableExtra)
library(performance)
library(see)
library(patchwork)
library(usethis)
library(devtools)
library(knitr)
library(emmeans)
library(here)
library(sjPlot)
library(gtsummary)
library(knitr)
library(rphylopic)

#------------------------------ Experiment 3 ---- 
#----- (Exp3) Day 1 ------
#-------- Reading the data in
feedinge3d1 <- read_excel("data/RPFemaleFeedingE3D1.xlsx")
#---- Making the data long
long_feedinge3d1 <- feedinge3d1 %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 1 
exp3feeding_summary <- long_feedinge3d1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for feeding day 1 ----------------#
exp3feeding_plotd1 <- exp3feeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge3d1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_minimal() 


#-------------- (Exp 3) Day 1 Data analysis  -----------

#------- creating a linear model for day 1 
exp3lm <- lm(fly_numbers ~ diet, data = long_feedinge3d1)
#------- using summary function for the model 
summary(exp3lm)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp3lm, specs = pairwise ~ diet) 

#--  1:8 soft and 1:8 hard is only just significant - prefer soft to feed on 
#-- but don't really like 8:1 soft 