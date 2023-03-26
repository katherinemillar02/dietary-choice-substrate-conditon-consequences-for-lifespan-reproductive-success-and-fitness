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
# summary of just day 2 
exp3feeding_summary_d1 <- long_feedinge3d1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for feeding day 1 ----------------#
exp3feeding_plotd1 <- exp3feeding_summary_d1 %>% 
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


#----- (Exp3) Day 2 ------
#-------- Reading the data in
feedinge3d2 <- read_excel("data/RPFemaleFeedingE3D2.xlsx")
#---- Making the data long
long_feedinge3d2 <- feedinge3d2 %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 1 
exp3feeding_summary_d2 <- long_feedinge3d2 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for feeding day 2 ----------------#
exp3feeding_plotd2 <- exp3feeding_summary_d2 %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge3d2,
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

#-------------- (Exp 3) Day 2 Data analysis  -----------

#------- creating a linear model for day 2 
exp3lm_d2 <- lm(fly_numbers ~ diet, data = long_feedinge3d2)
#------- using summary function for the model 
summary(exp3lm)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp3lm_d2, specs = pairwise ~ diet) 


#-------- visualising the feeding data for different days together using patchwork
exp3feeding_plotd1 + exp3feeding_plotd2


#--------------  Combined days data  ----

#--- summarising the data with combined days 
exp3feeding_summary_both <- exp3both %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#-------------- combined day plot for data visualisation 

exp3feeding_plot_both <- exp3feeding_summary_both %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp3both,
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


#--- mutating a day variable 
exp3d1 <- long_feedinge3d1 %>% mutate(day = "1")
exp3d2 <- long_feedinge3d2 %>% mutate(day = "2")
#--- combining the two days 
exp3both <- rbind(exp3d1, exp3d2)


#-- making a linear model for the combined days model with day in the model 
exp3bothlm <- lm(fly_numbers ~ diet + day, data = exp3both)

#-- summaryb function of combined days linear model 
summary(exp3bothlm)
#-- checking for the significance in day in the model
drop1(exp3bothlm, test = "F")

#--- summarising the data with combined days 
exp3feeding_summary_both <- exp3both %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#--- two-factor feeding analysis ------

# splitting up hard and soft diets and differernt nutrient diets 
exp3both$food_type <- ifelse(exp3both$diet %in% c("8:1H", "1:8H"), "hard", "soft")

exp3both$food_nutrition <- ifelse(exp3both$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

# viewing the new dataset
view(exp3both)


# creating a linear model based on food nutrition and food type 
exp3bothlmnew <- lm(fly_numbers ~ food_type + food_nutrition, data = exp3both)

# creating a linear model based on food nutrition and food type with an interaction effect 
exp3bothlmnew2 <- lm(fly_numbers ~ food_type + food_nutrition + food_nutrition * food_type, data = exp3both)

# summarising the linear models 
summary(exp3bothlmnew)
summary(exp3bothlmnew2)

# -------- (Exp 3) Egg counting  --------

#____ Reading the data in 
egg_counting_data_e3 <- (read_excel(path = "data/RPEggCountE3.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting3 <- egg_counting_data_e3 %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting3_summary <- long_egg_counting3 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
egg_counting3_plot <- egg_counting3_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch")+
  theme_minimal()

eggcountinge3ls1 <- lm(egg_numbers ~ diet, data = long_egg_counting3)
emmeans::emmeans(eggcountinge3ls1, specs = pairwise ~ diet)
summary(eggcountinge3ls1)
