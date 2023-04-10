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

#------------------------------ Experiment 1a ----

# The first experiment using diets 1:2 and 1:8 
#----- (Exp1a) Day 1 ------
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
exp1feeding_plotd1 <- exp1feeding_summary %>% 
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

#-------------- Data analysis (Exp 1a) Day 1   -----------
#------- creating a linear model for day 1 
exp1lm <- lm(fly_numbers ~ diet, data = long_feedinge1d1)
#------- using summary function for the model 
summary(exp1lm)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp1lm, specs = pairwise ~ diet) 


#------------ (Exp 1a) Day 2 ------
#-------- Reading the data in
feedinge1d2 <- read_excel("data/RPFemaleFeedingE1D2.xlsx")
#---- Making the data long
long_feedinge1d2 <- feedinge1d2 %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 2 
exp1d2feeding_summary <- long_feedinge1d2 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#- visualising the data for just day 2 
exp1feeding_plotd2 <- exp1d2feeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge1d2,
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

# (Exp1a) ------- Day 2 Data analysis 
# Data analysis for just day 2 
#------- creating a linear model for day 1 
exp1lmd2 <- lm(fly_numbers ~ diet, data = long_feedinge1d2)
#------- using summary function for the model 
summary(exp1lmd2)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp1lmd2, specs = pairwise ~ diet) 

#------- comparing the days using patchwork

exp1feeding_plotd1 + exp1feeding_plotd2

#----- (Exp1a) Combined days data ---------

# combining days 1 and 2 for overall data analysis 
#------- Combining the data for feeding behaviour 

#------- Mutating a variable for day 
exp1d1 <- long_feedinge1d1 %>% mutate(day = "1")
exp1d2 <- long_feedinge1d2 %>% mutate(day = "2")
#------- Combining the days 
exp1a_all <- rbind(exp1d1, exp1d2)
# summarising the combined days data 
exp1a_all_summary <- exp1a_all %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#  Table of the combined days data for experiment 1a
exp1a_all %>% 
  group_by(diet) %>% 
  summarise(`Mean fly numbers`= mean(fly_numbers, na.rm = T),
            `SD`= sd(fly_numbers, na.rm = T)) %>% gt::gt()


# visualising the data for combined days 
exp1all_plot <- exp1a_all_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1a_all,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 4.0)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch")+
  theme_minimal() 

# ------ Exp 1a Combining Analysis -----

# ------ -(Exp1a) Combined days data analysis ----------



# making a model with just day in for analysis 
exp1a_all_day_lm <- lm(fly_numbers ~ day, data = exp1a_all)

# checking the experiment 1a just day model
performance::check_model(exp1a_all_day_lm)
performance::check_model(exp1a_all_day_lm, check = c("qq"))

# making a glm model with just day in for analysis 
exp1a_all_day_glm <- glm(fly_numbers ~ day, family = poisson, data = exp1a_all)

# using summary to check for overdispersion
summary(exp1a_all_day_glm)

# model is overdispersed so using quasipoisson
exp1a_all_day_glm_2 <- glm(fly_numbers ~ day, family = quasipoisson, data = exp1a_all)

# checking the experiment 1a just day model (glm)
performance::check_model(exp1a_all_day_glm_2)
performance::check_model(exp1a_all_day_glm_2, check = c("qq", "homogeneity"))

# qq looks similar but maybe glm one looks slightly better 
# homogenity also looks better with glm 
# I think exp1a_all_day_glm_2 is an okay model to use? 



# Using drop1 to look for significance of day in day glm model 
drop1(exp1a_all_day_glm_2, test = 'F')

# using summary to look at significance of day
summary(exp1a_all_day_glm_2)
# if doing anova analysis for this 
# why so  different p values ?? 

# ---------------- Experiment 1b - repeating the experiment -----
#----- (Exp1b) Day 1 ----
#-------- Reading the data in
feedinge1bd1 <- read_excel("data/RPFemaleFeedingE1bD1.xlsx")
#---- Making the data long
long_feedinge1bd1 <- feedinge1bd1 %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 1 
exp1bfeeding_summary <- long_feedinge1bd1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
#------- Visualising the data for feeding day 1 ----------------#
exp1bfeeding_plotd1 <- exp1bfeeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge1bd1,
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



#----- Day 2 
#-------- Reading the data in
feedinge1bd2 <- read_excel("data/RPFemaleFeedingE1bD2.xlsx")
#---- Making the data long
long_feedinge1bd2 <- feedinge1bd2 %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 2 
exp1bd2feeding_summary <- long_feedinge1bd2 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#- visualising the data for just day 2 
exp1bfeeding_plotd2 <- exp1bd2feeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge1bd2,
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


#------- comparing the days using patchwork

exp1bfeeding_plotd1 + exp1bfeeding_plotd2

#-----  (Exp1b) data analysis -----
#- Data analysis of combined days (experiment 1b)

#------- Mutating a variable for day 
exp1bd1 <- long_feedinge1bd1 %>% mutate(day = "1") 
exp1bd2 <- long_feedinge1bd2 %>% mutate(day = "2")

#------- Combining the days 
exp1ball <- rbind(exp1bd1, exp1bd2)
# summarising the combined days data 
exp1ball_summary <- exp1ball %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

# visualising the data for combined days 
exp1ball_plot <- exp1ball_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1ball,
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



# creating a model with just day in for analysis 
exp1balllmday <- lm(fly_numbers ~ day, data = exp1ball)
# using performance::check for the new model
performance::check_model(exp1balllmday)
performance::check_model(exp1balllmday, check = c("qq"))

# trying to fix the model which checks for day 
exp1balllmday2 <- lm(formula = (fly_numbers + 1) ~ day, data = exp1ball)

# using performance::check to check the new day model
performance::check_model(exp1balllmday2)
performance::check_model(exp1balllmday2, check = c("qq"))
# normality looks exactly the same even with + 1 

# day model doesn't look too good - what to do? 

# trying a glm
exp1balllmdayglm <- glm(fly_numbers ~ day, family = poisson, data = exp1ball)

# summarising glm
summary(exp1balllmdayglm)

#  more than 1 so do quaspoisson 
# trying a glm 2 
exp1balldayglm2 <- glm(fly_numbers ~ day, family = quasipoisson, data = exp1ball)

# summarising glm 2
summary(exp1balllmdayglm2)

# usng performance::check on the glm2
performance::check_model(exp1balllmdayglm2)
performance::check_model(exp1balllmdayglm2, check = c("qq"))

# trying to fix the glm model which checks for day 
exp1ballglmday20 <- glm(formula = (fly_numbers + 1) ~ day, family = quasipoisson, data = exp1ball)

# using performance::check on the glm20
performance::check_model(exp1ballglmday20)
performance::check_model(exp1ballglmday20, check = c("qq"))

# normality might look worse but glm generally looks better than lm? 
# use the glm2 (that hasn't been added +1)


#trying drop1 of the right day model of experiment 1b to do day analysis 
drop1(exp1balldayglm2, test = "F")
# is this okay 

# summarising the new day glm 2
summary(exp1balldayglm2)

# Combining experiments ----
# combining the two repeat experiments

# adding a variable for experments 1a and 1b 
exp1a <- exp1a_all %>% mutate(experiment = "exp1a") 
exp1b <- exp1ball %>% mutate(experiment = "exp1b")

#  binding experiment 1a and 1b together into one data-set 
exp1_combined <- rbind(exp1a, exp1b)

# vewing the new data set 
view(exp1_combined)

# summarising the combined experiments data 
exp1_combined_summary <- exp1_combined %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

# visualising the data for combined days 
exp1_combined_plot <- exp1_combined_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1_combined,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 


# Data analysis: Combining experiments - feeding behaviour -----


# Testing a model for feeding behaviour
# JUST experiment model for data analysis - so i can make sense of it and using it 
exp1_combined_experiment <- lm(fly_numbers ~ experiment, data = exp1_combined)

# performance check for experiment linear model of combined experiments data 
performance::check_model(exp1_combined_experiment)
performance::check_model(exp1_combined_experiment, check = c("qq"))

# looks not the best 
# trying a glm 
exp1_combined_experiment_glm <- glm(fly_numbers ~ experiment, family = poisson, data = exp1_combined)

# checking if poisson is right fit with summary()
summary(exp1_combined_experiment_glm)

# poisson is overdispersed so using quasipoisson 
exp1_combined_experiment_glm_2 <- glm(fly_numbers ~ experiment, family = quasipoisson, data = exp1_combined)

# performance check for experiment generalised linear model of combined experiments data 
performance::check_model(exp1_combined_experiment_glm_2)
performance::check_model(exp1_combined_experiment_glm_2, check = c("qq"))

# glm looks better but normality still looks dodgy 
# the normality on glm looks the same as lm 
# but the homogenity on glm looks better 


# trying the (fly_numbers + 1) to improve the model? 
exp1_combined_experiment_glm_3 <- glm(formula = (fly_numbers + 1) ~ experiment, family = quasipoisson, data = exp1_combined)

# performance check for experiment new generalised linear model 3 of combined experiments data 
performance::check_model(exp1_combined_experiment_glm_3)
performance::check_model(exp1_combined_experiment_glm_3, check = c("qq"))

# any number I add doesn't seem to be changing the qq plot?? 
#  trying with log - even though glm is log anyway right 
exp1_combined_experiment_glm_4 <- glm(formula = log(fly_numbers + 1) ~ experiment, family = quasipoisson, data = exp1_combined)

#  performance checking model 4 
performance::check_model(exp1_combined_experiment_glm_4)
performance::check_model(exp1_combined_experiment_glm_4, check = c("qq"))

# from my analysis I think the glm 3 model was the best
# not sure how to change normality to look better though? 

# drop1 f the chosen experiment model for experiment analysis
drop1(exp1_combined_experiment_glm_3, test = "F")

# summary function of the chosen experiment model for experiment analysis 
summary(exp1_combined_experiment_glm_3)


# doing data analysis with experiment not included in a model 
exp1_combined_lm <- lm(fly_numbers ~ diet, data = exp1_combined)


# performance checking model 4 
performance::check_model(exp1_combined_lm)
performance::check_model(exp1_combined_lm, check = c("qq"))

# model doesn't look the best
# trying glm 
exp1_combined_glm <- glm(fly_numbers ~ diet, family = poisson(), data = exp1_combined)
# checking if glm is dispersed or not
summary(exp1_combined_glm)
# overdispersion so using quasipoisson
exp1_combined_glm_2 <- glm(fly_numbers ~ diet, family = quasipoisson(), data = exp1_combined)

# performance checking with the new model
performance::check_model(exp1_combined_glm_2)
performance::check_model(exp1_combined_glm_2, check = c("qq"))
# issues at the beginning and end 

exp1_combined_glm_3 <- glm(formula = (fly_numbers + 1) ~ diet, family = quasipoisson, data = exp1_combined)

# performance checking with the new model
performance::check_model(exp1_combined_glm_3)
performance::check_model(exp1_combined_glm_3, check = c("qq"))

# both homogenty and normality look worse with +1 
# don't know how to fix beginning bit of normality - seems to be a reccuring issue 
# best model of these seems to be exp1_combined_glm_2 

# Using summary function for analysis 
summary(exp1_combined_glm_2)

# using em-means to test everything
emmeans::emmeans(exp1_combined_glm_2, specs = pairwise ~ diet)



# Two factor analysis - overall experiments for experiment 1 two factor analysis -------


# adding the variables to add new columns to the dataset 
exp1_combined$food_type <- ifelse(exp1both$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
exp1_combined$food_nutrition <- ifelse(exp1both$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")

# visualising the data for soft and hard vs nutrient composition
# summarising hard vs soft data 
softhard_summary_exp1 <- exp1_combined%>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# a soft vs hard plot 
softhard_plot_exp1 <- softhard_summary_exp1 %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1_combined,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 
# summarising nutrient composition data 
nutrient_summary_exp1 <- exp1_combined %>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# a nutrient plot 
nutrient_plot_exp1 <- nutrient_summary_exp1 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1_combined,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 

# using patchwork to compare soft/hardness and nutrient composition - data visualisation
softhard_plot_exp1 + nutrient_plot_exp1


#  Data analysis --- two factor analysis - food conditions 


# making a linear model for food conditions 
exp1_combined_foodconditions_lm <- lm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1_combined)

# checking the model 
performance::check_model(exp1_combined_foodconditions_lm)
performance::check_model(exp1_combined_foodconditions_lm, check = c("qq"))

# trying a glm 
exp1_combined_foodconditions_glm <- glm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = poisson(), data = exp1_combined)

# using the summary function to check for overdispersion
summary(exp1_combined_foodconditions_glm)

# overdispersed so use quasipoisson
exp1_combined_foodconditions_glm2 <- glm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson(), data = exp1_combined)

# checking the new generalised linear model 
performance::check_model(exp1_combined_foodconditions_glm2)
performance::check_model(exp1_combined_foodconditions_glm2, check = c("qq"))

# linear model is probably better so far 

# adding 1 to fly numbers to change the lm 

exp1_combined_foodconditions_lm2 <- lm(formula = (fly_numbers + 1) ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1_combined)

# putting the model in log 
exp1_combined_foodconditions_lm3 <- lm(formula = log(fly_numbers + 1) ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1_combined)

# checking the new  linear model 
performance::check_model(exp1_combined_foodconditions_lm2)
performance::check_model(exp1_combined_foodconditions_lm2, check = c("qq"))
# worse than the original linear model 

# checking the new  linear model 3
performance::check_model(exp1_combined_foodconditions_lm3)
performance::check_model(exp1_combined_foodconditions_lm3, check = c("qq"))

# normality looks a lot better with the linear model in log + 1
# final choice = exp1_combined_foodconditions_lm3

# using summary to do data analysis 
summary(exp1_combined_foodconditions_lm3)



#---- Combined experiments egg data ----
#------- collating egg counting data to look for significance 
#--- adding an experiment variable to egg counting data 
exp1aegg <- long_egg_counting1 %>% mutate(experiment = "exp1a")  
exp1begg <- long_egg_counting1b %>% mutate(experiment = "exp1b")
#---  combining the data 
exp1_egg_combined <- rbind(exp1aegg, exp1begg)

#---- Combined experiments egg data analysis ----
# summary of all combined egg data 
exp1_egg_combined_summary <- exp1_egg_combined %>%  
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
# plot of all combined egg data 
exp1_egg_combined_plot <- exp1_egg_combined_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = exp1_egg_combined,
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

# making a model to be looking for just the significance in experiment 
exp1_egg_combined_experiment <- lm(egg_numbers ~ experiment, data = exp1_egg_combined)

# checking the model 
performance::check_model(exp1_egg_combined_experiment)
performance::check_model(exp1_egg_combined_experiment, check = c("qq"))
# normality could be better 
# linearity and homogenity don't look the best

# trying a generalised linear model
exp1_egg_combined_experiment_glm <- glm(egg_numbers ~ experiment, family = poisson, data = exp1_egg_combined)

# using summary to check for overdispersion
summary(exp1_egg_combined_experiment_glm)
# very very overdispersed

exp1_egg_combined_experiment_glm_2 <- glm(egg_numbers ~ experiment, family = quasipoisson, data = exp1_egg_combined)

#  checking the model 
performance::check_model(exp1_egg_combined_experiment_glm_2)
performance::check_model(exp1_egg_combined_experiment_glm_2, check = c("qq"))
#  looks a lot better 
#  apart from normality at the beginning 
# best so far is exp1_egg_combined_experiment_glm_2 so use this 

# using drop1 to see significance of experiment 
drop1(exp1_egg_combined_experiment_glm_2, test = "F")

summary(exp1_egg_combined_experiment_glm_2)




#---  linear model for collated egg counting data 
exp1_egg_combined_lm <- lm(egg_numbers ~ diet, data = exp1_egg_combined)

#  checking the model 
performance::check_model(exp1_egg_combined_lm)
performance::check_model(exp1_egg_combined_lm, check = c("qq"))
# doesn't look the best 

#---  generalised linear model for collated egg counting data 
exp1_egg_combined_glm <- glm(egg_numbers ~ diet, family = poisson, data = exp1_egg_combined)

summary(exp1_egg_combined_glm)

exp1_egg_combined_glm_2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = exp1_egg_combined)

#  checking the model 
performance::check_model(exp1_egg_combined_glm_2)
performance::check_model(exp1_egg_combined_glm_2, check = c("qq"))
# normality looks okay? 

# trying + 1 on the glm
exp1_egg_combined_glm_3 <- glm(formula = (egg_numbers + 1) ~ diet, family = quasipoisson, data = exp1_egg_combined)

#  checking the model 
performance::check_model(exp1_egg_combined_glm_3)
performance::check_model(exp1_egg_combined_glm_3, check = c("qq"))
#  doesn't make too much difference to homogenity or normality 
# final choice anyway: exp1_egg_combined_glm_3

# analysing with the chosen model 
summary(exp1_egg_combined_glm_3)

# using emmeans tukey to analyse everything 
emmeans::emmeans(exp1_egg_combined_glm_3, specs = pairwise ~ diet) 


# OVIPOSTION DATA - TWO FACTOR ANALYSIS 
exp1_egg_combined$food_type <- ifelse(eggboth$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
exp1_egg_combined$food_nutrition <- ifelse(eggboth$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")
# viewing the new dataset 
view(exp1_egg_combined)

# visualising the data for data analysis 
# summarising hard vs soft data 
softhard_summary_exp1_egg <- exp1_egg_combined %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
# a soft vs hard plot 
softhard_plot_exp1_egg <- softhard_summary_exp1_egg %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = exp1_egg_combined,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch",
       title = "")+
  theme_classic() 

# summarising nutrient composition data 
nutrient_summary_exp1_egg <- exp1_egg_combined %>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
# a nutrient plot 
nutrient_plot_exp1_egg <- nutrient_summary_exp1_egg %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = exp1_egg_combined,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch",
       title = "")+
  theme_classic() 

# using patchwork to compare soft/hardness and nutrient composition - data visualisation
softhard_plot_exp1_egg + nutrient_plot_exp1_egg

# OVIPOSITION TWO FACTOR DATA ANALYSIS -----

# trying a linear model
exp1_combined_egg_foodcondition_lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1_egg_combined)

# checking the model 
performance::check_model(exp1_combined_egg_foodcondition_lm)
performance::check_model(exp1_combined_egg_foodcondition_lm, check = c("qq"))
performance::check_model(exp1_combined_egg_foodcondition_lm, check = c("linearity"))
# normality looks very poor 
# for linearity the line is flat but there is a slope 

# trying a glm
exp1_combined_egg_foodcondition_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = poisson, data = exp1_egg_combined)

# checking for overdispersion with the glm
summary(exp1_combined_egg_foodcondition_glm)
# overdispersed so using quasipoisson 

exp1_combined_egg_foodcondition_glm2 <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = exp1_egg_combined)

# checking the model 
performance::check_model(exp1_combined_egg_foodcondition_glm2)
performance::check_model(exp1_combined_egg_foodcondition_glm2, check = c("qq"))
# qq looks a lot better
# homogenity loooks ok? 

# analysing the chosen egg food condition model
summary(exp1_combined_egg_foodcondition_glm2)
drop1(exp1_combined_egg_foodcondition_glm2, test = "F")
























# THIS CODE IS NOT USED IN OVERALL ANALYSIS 
# CAN IGNORE FOR NOW
## JUST THERE FOR KNOWLEDGE 
anova(exp1alllm)
#using em means to test everything - tukey 
emmeans::emmeans(exp1alllm, specs = pairwise ~ diet + day) 
# testing for significance in day and diet 
drop1(exp1alllm, test = "F")
# model without day in 
exp1alllm2 <- lm(fly_numbers ~ diet, data = exp1all)
summary(exp1alllm2)
drop1(exp1alllm2, test = "F")
broom::tidy(exp1alllm2)
exp1allglm01 <- glm(fly_numbers ~ diet, data = exp1all, family = poisson)
exp1allglm <- glm(fly_numbers ~ diet, data = exp1all, family = quasipoisson)
summary(exp1allglm)
summary(exp1allglm01)
exp1allglm %>% broom::tidy(conf.int = T) %>% 
  select(-`std.error`) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kbl(col.names = c("Predictors",
                    "Estimates",
                    "Z-value",
                    "P",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Generalised linear model coefficients", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=16)
# why is p value 0? 



# -------- Exp1a - Two factor analysis feeding 

# THIS CODE IS NOT USED IN OVERALL ANALYSIS 
# CAN IGNORE FOR NOW
## JUST THERE FOR KNOWLEDGE 
exp1all$food_type <- ifelse(exp1all$diet %in% c("1:8H", "1:2H"), "hard", "soft")
exp1all$food_nutrition <- ifelse(exp1all$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")
view(exp1all)
exp1alllmnew <- lm(fly_numbers ~ food_type + food_nutrition, data = exp1all)
summary(exp1alllmnew)
exp1alglmnew <- glm(fly_numbers ~ food_type + food_nutrition, family = quasipoisson(), data = exp1all)
summary(exp1alglmnew)
performance::check_model(exp1alllmnew)
performance::check_model(exp1alglmnew)
exp1alllmnew2 <- lm(fly_numbers ~ food_type + food_nutrition + food_type * food_nutrition, data = exp1all)
summary(exp1alllmnew2)
exp1alglmnew2 <- glm(fly_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = quasipoisson(), data = exp1all)
summary(exp1alglmnew2)
# THIS CODE IS NOT USED IN OVERALL ANALYSIS 
# CAN IGNORE FOR NOW
## JUST THERE FOR KNOWLEDGE 
# -------- (Exp 1a) Egg counting  
#____ Reading the data in 
egg_counting_data <- (read_excel(path = "data/RPEggCountE1.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting1 <- egg_counting_data %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting1_summary <- long_egg_counting1 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
egg_counting1_plot <- egg_counting1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting1,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch", 
       title = "Mated Female Oviposition Preference")+
  theme_minimal()
# THIS CODE IS NOT USED IN OVERALL ANALYSIS 
# CAN IGNORE FOR NOW
## JUST THERE FOR KNOWLEDGE 
#------- (Exp1a) Egg counting data analysis 
#-- Making a linear model 
eggcountinge1ls1 <- lm(egg_numbers ~ diet, data = long_egg_counting1)
#---- Checking the model 
performance::check_model(eggcountinge1ls1)
#---- summarising the data 
summary(eggcountinge1ls1)
#----  doing tests 
anova(eggcountinge1ls1) 
confint(eggcountinge1ls1)
# tidyverse summary
broom::tidy(eggcountinge1ls1,  
            exponentiate=T, 
            conf.int=T)
# Data analysis of egg counting from experiment 1 
emmeans::emmeans(eggcountinge1ls1, specs = pairwise ~ diet) 
eggcountinge1ls2 <- glm(egg_numbers ~ diet, data = long_egg_counting1, family = quasipoisson)
summary(eggcountinge1ls2)
emmeans::emmeans(eggcountinge1ls2, specs = pairwise ~ diet) 
# No significance between soft and hard diets for egg laying 
# summary says there is a difference 



--------# two factor analysis egg 
# THIS CODE IS NOT USED IN OVERALL ANALYSIS 
# CAN IGNORE FOR NOW
## JUST THERE FOR KNOWLEDGE 
# Using summary function for analysis 
summary(exp1balllm)
summary(exp1balllmday)
# using em means to test everything
emmeans::emmeans(exp1balllm, specs = pairwise ~ diet + day) 
# testing for significance in day 
drop1(exp1balllm, test = "F")
drop1(exp1balllmday, test = "F")
exp1balllm2 <- lm(fly_numbers ~ diet, data = exp1ball)
summary(exp1balllm2)


# (Exp1b) Egg count data analysis 

#____ Reading the data in 
egg_counting_data_1b <- (read_excel(path = "data/RPEggCountE1b.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting1b <- egg_counting_data_1b %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting1_summary_1b <- long_egg_counting1b %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
egg_counting1b_plot <- egg_counting1_summary_1b %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting1b,
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

#-- using patchwork to compare the egg plots 

egg_counting1_plot + egg_counting1b_plot

#-- Egg counting data analysis 

#-- Making a linear model 
eggcountinge1bls1 <- lm(egg_numbers ~ diet, data = long_egg_counting1b)
#---- Checking the model 
rmance::check_model(eggcountinge1bls1)
#---- summarising the data 
summary(eggcountinge1bls1)
#----  doing tests 
anova(eggcountinge1bls1) 
confint(eggcountinge1bls1)
#tidyverse summary
broom::tidy(eggcountinge1bls1,  
            exponentiate=T, 
            conf.int=T)

emmeans::emmeans(eggcountinge1bls1, specs = pairwise ~ diet)

# comparing the repeats of experiment 1a
# using patchwork to compare experiment 1a and experiment 1b 
exp1all_plot + exp1ball_plot



install.packages("ggpubr")
library(ggpubr)

egg_counting_plot_all + softhard_plot_exp1_egg + nutrient_plot_exp1_egg 


