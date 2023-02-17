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

#----- Day 1 
#-------- Reading the data in
feedinge1d1 <- read_excel("data/RPFemaleFeedingE1D1.xlsx")
#---- Making the data long
long_feedinge1d1 <- feedinge1d1 %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 1 
exp1feeding_summary <- long_feedinge1d1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for feeding day 1 ----------------#
exp1feeding_plot <- exp1feeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge1d1,
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

#------- creating a linear model 
exp1lm <- lm(fly_numbers ~ diet, data = long_feedinge1d1)
#------- using summary function for the model 
summary(exp1lm)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp1lm, specs = pairwise ~ diet) 


#----- Day 2 
#-------- Reading the data in
feedinge1d2 <- read_excel("data/RPFemaleFeedingE1D2.xlsx")
#---- Making the data long
long_feedinge1d2 <- feedinge1d2 %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 2 
exp2feeding_summary <- long_feedinge1d2 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#------- Mutating a variable for day 
exp1d1 <- long_feedinge1d1 %>% mutate(day = "1")
exp1d2 <- long_feedinge1d2 %>% mutate(day = "2")
#------- Combining the days 
exp1all <- rbind(exp1d1, exp1d2)

exp1all_summary <- exp1all %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

exp1all_plot <- exp1all_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1all,
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



exp1alllm <- lm(fly_numbers ~ diet + day, data = exp1all)

summary(exp1alllm)

emmeans::emmeans(exp1alllm, specs = pairwise ~ diet + day) 


drop1(exp1alllm, test = "F")
