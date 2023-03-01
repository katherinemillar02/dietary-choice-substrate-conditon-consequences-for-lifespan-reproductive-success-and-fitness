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


# Feeding behaviour day 1 

#-------- Reading the data in
feedinge2d1 <- read_excel("data/RPFemaleFeedingE2D1.xlsx")
#---- Making the data long
long_feedinge2d1 <- feedinge2d1 %>% 
  pivot_longer(cols = ("8:1S":"1:2H"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 1 
exp2feeding_summary_d1 <- long_feedinge2d1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------- Visualising the data for feeding day 1 ----------------#
exp2feeding_plot_d1<- exp2feeding_summary_d1 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge2d1,
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

#-------------- (Exp 2) Day 1 Data analysis  -----------

#------- creating a linear model for day 1 
exp2lm <- lm(fly_numbers ~ diet, data = long_feedinge2d1) 
#------- using summary function for the model 
summary(exp2lm)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp2lm, specs = pairwise ~ diet) 

#------------ (Exp 2) Day 2 ------
#-------- Reading the data in
feedinge2d2 <- read_excel("data/RPFemaleFeedingE2D2.xlsx")
#---- Making the data long
long_feedinge2d2 <- feedinge2d2 %>% 
  pivot_longer(cols = ("8:1S":"1:2H"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 2 
exp2d2feeding_summary <- long_feedinge2d2 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

exp2feeding_plotd2 <- exp2d2feeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge2d2,
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

#-------------- (Exp 2) Day 2 Data analysis  -----------

#------- creating a linear model for day 1 
exp2lmd2 <- lm(fly_numbers ~ diet, data = long_feedinge2d2) 
#------- using summary function for the model 
summary(exp2lmd2)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp2lmd2, specs = pairwise ~ diet) 



exp2feeding_plot_d1 + exp2feeding_plotd2

