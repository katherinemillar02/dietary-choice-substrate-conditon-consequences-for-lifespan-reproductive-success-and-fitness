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

#--------------   ANALYSIS TO BE USED -----
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
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 


#--- mutating a day variable 
exp3d1 <- long_feedinge3d1 %>% mutate(day = "1")
exp3d2 <- long_feedinge3d2 %>% mutate(day = "2")
#--- combining the two days 
exp3_combined <- rbind(exp3d1, exp3d2)

#--- data analysis for combined data ----

#-- making a linear model for day analysis 
exp3_combined_day_lm <- lm(fly_numbers ~ day, data = exp3_combined)

# checking the model 
performance::check_model(exp3_combined_day_lm)
performance::check_model(exp3_combined_day_lm, check = c("qq"))
performance::check_model(exp3_combined_day_lm, check = c("linearity"))
# linearity looks not great

#-- making a gernealised linear model for day analysis 
exp3_combined_day_glm <- glm(fly_numbers ~ day, family = poisson, data = exp3_combined)

summary(exp3_combined_day_glm)

exp3_combined_day_glm2 <- glm(fly_numbers ~ day, family = quasipoisson, data = exp3_combined)


# checking the model 
performance::check_model(exp3_combined_day_glm2)
performance::check_model(exp3_combined_day_glm2, check = c("qq"))
# homogenity looks more normal
# normality looks slightly worse but still okay 
# stick with this exp3_combined_day_glm2

# analysing the data for the chosen model 

#-- summary function of combined days linear model 
summary(exp3_combined_day_glm2)
#-- checking for the significance in day in the model
drop1(exp3_combined_day_glm2, test = "F")


#- making a model for fly numbers and diet
exp3_combined_lm <- lm(fly_numbers ~ diet, data = exp3_combined)

# checking the model 
performance::check_model(exp3_combined_lm)
performance::check_model(exp3_combined_lm, check = c("qq"))
performance::check_model(exp3_combined_lm, check = c("linearity"))
# looks ok
# stick with this as  chosen model 

# data analysis for chosen model

# using summary() which will show anova
summary(exp3_combined_lm)

# using emmeans tukey to test everything 
emmeans::emmeans(exp3_combined_lm, specs = pairwise ~ diet)


#--- TWO FACTOR FEEDING ANALYSIS --- FOOD CONDITIONS 

# splitting up hard and soft diets and different nutrient diets 
exp3_combined$food_type <- ifelse(exp3_combined$diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
exp3_combined$food_nutrition <- ifelse(exp3_combined$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

# viewing the new dataset
view(exp3_combined)



# summarising hard vs soft data 
softhard_summary_exp3 <- exp3both %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# a soft vs hard plot 
softhard_plot_exp3 <- softhard_summary_exp3 %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp3both,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 
# summarising nutrient composition data 
nutrient_summary_exp3 <- exp3both %>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# a nutrient plot 
nutrient_plot_exp3 <- nutrient_summary_exp3 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp3both,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 
# using patchwork to compare soft/hardness and nutrient composition - data visualisation
softhard_plot_exp3 + nutrient_plot_exp3

#--- DATA ANALYSIS - TWO FACTOR FEEDING ANALYSIS --- FOOD CONDITIONS

# creating a linear model based on food nutrition and food type 
exp3_combined_foodcondition_lm <- lm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp3_combined)

performance::check_model(exp3_combined_foodcondition_lm)
performance::check_model(exp3_combined_foodcondition_lm, check = c("qq"))
performance::check_model(exp3_combined_foodcondition_lm, check = c("linearity"))
# looks ok? 

#  using chosen model for data analysis 
summary(exp3_combined_foodcondition_lm)


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
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch")+
  theme_classic()

#-- linear model of egg counting 
exp3_egg_lm <- lm(egg_numbers ~ diet, data = long_egg_counting3)

performance::check_model(exp3_egg_lm)
performance::check_model(exp3_egg_lm, check = c("qq"))
performance::check_model(exp3_egg_lm, check = c("linearity"))
# not a good model 

# trying a glm 
exp3_egg_glm <- glm(egg_numbers ~ diet, family = poisson, data = long_egg_counting3)

summary(exp3_egg_glm)

exp3_egg_glm2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting3)


performance::check_model(exp3_egg_glm2)
performance::check_model(exp3_egg_glm2, check = c("qq"))
# qq looks better
#  homogenity looks ok 
# stick with this 

#  using the chosen model for data analysis 
# summary function to look at anova 
summary(exp3_egg_glm2)

#-- analysing all egg data using tukey emmeans 
emmeans::emmeans(exp3_egg_glm2, specs = pairwise ~ diet)



#--------- TWO-FACTOR DATA ANALYSIS -- EGG COUNTING - EXPERIMENT 3 
# changing the data to columns 
long_egg_counting3$food_type <- ifelse(long_egg_counting3 $diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
long_egg_counting3$food_nutrition <- ifelse(long_egg_counting3 $diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

# code to view the new data
view(long_egg_counting3)

# Visualising the data soft and hard 
softhardegg_summary_exp3 <- long_egg_counting3 %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#- Making a plot of egg counting// soft and hard 
softhardegg_plot_exp3 <- softhardegg_summary_exp3 %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 110)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs on a patch",
       title = "")+
  theme_minimal() 

# summarising egg nutrient composition 
nutrientegg_summary_exp3 <- long_egg_counting3%>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

# a nutrient plot 
nutrientegg_plot_exp3 <- nutrientegg_summary_exp3 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 110)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs on a patch",
       title = "")+
  theme_minimal() 


# combining the experiment hardness plot with the nutrient plot with patchwork 
softhardegg_plot_exp3 + nutrientegg_plot_exp3

eggexp3lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, data = long_egg_counting3)
summary(eggexp3lm)

eggexp3glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = poisson(), data = long_egg_counting3)
summary(eggexp3glm)

eggexp3glm2 <- glm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = quasipoisson(), data = long_egg_counting3)
summary(eggexp3glm2)



performance::check_model(eggexp3lm)
performance::check_model(eggexp3glm2)

performance::check_model(eggexp3lm, check = c("qq"))

performance::check_model(eggexp3glm2, check = c("qq"))
