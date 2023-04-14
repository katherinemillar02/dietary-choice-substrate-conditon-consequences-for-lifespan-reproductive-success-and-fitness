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
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch", 
       title = "Day 1")+
  theme_classic() 

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
exp3feeding_summary_d1

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
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "Day 2")+
  theme_classic() 

exp3feeding_plotd1 + exp3feeding_plotd2


long_feedinge3d2$food_type <- ifelse(long_feedinge3d2$diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
long_feedinge3d2$food_nutrition <- ifelse(long_feedinge3d2$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

long_feedinge3d1$food_type <- ifelse(long_feedinge3d1$diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
long_feedinge3d1$food_nutrition <- ifelse(long_feedinge3d1$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")




long_feedinge3d1

exp3feeding_summary_d2_fh <- long_feedinge3d2 %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))



#-------------- (Exp 3) Day 2 Data analysis  -----------

#------- creating a linear model for day 2 
exp3lm_d2 <- lm(fly_numbers ~ diet, data = long_feedinge3d2)
#------- using summary function for the model 
summary(exp3lm)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp3lm_d2, specs = pairwise ~ diet) 


#-------- visualising the feeding data for different days together using patchwork
exp3feeding_plotd1 + exp3feeding_plotd2

#--------------   ANALYSIS TO BE USED ----------------
#--------------  Combined days data  ----

#--- mutating a day variable 
exp3d1 <- long_feedinge3d1 %>% mutate(day = "1")
exp3d2 <- long_feedinge3d2 %>% mutate(day = "2")
#--- combining the two days 
exp3_combined <- rbind(exp3d1, exp3d2)



#--- summarising the data with combined days 
exp3feeding_summary_both <- exp3_combined %>%  
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
  geom_jitter(data = exp3_combined,
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




#--- Data analysis for combined days data ----

# playing around 
exp3_combined <- exp3_combined %>% filter(fly_numbers <60)

performance::check_model(exp3_combined_day_lm, check = c("outliers"))

#-- making a linear model for day analysis 
exp3_combined_day_lm <- lm(fly_numbers ~ day, data = exp3_combined)

exp3_combined_day_lm_2 <- lm(fly_numbers ~ day * diet, data = exp3_combined)

#
performance::check_model(exp3_combined_day_lm_2)
performance::check_model(exp3_combined_day_lm_2, check = c("qq"))
performance::check_model(exp3_combined_day_lm_2, check = c("linearity"))
performance::check_model(exp3_combined_day_lm_2, check = c("outliers"))
# cooks distance = 0.9 - 
# slope - something hasn't been measured 
# more error at high than low values

MASS::boxcox(exp3_combined_day_lm_3)

exp3_combined_day_lm_3 <- lm(formula = (fly_numbers + 1) ~ day * diet, data = exp3_combined)

exp3_combined_day_lm_4 <- lm(formula = log(fly_numbers + 1) ~ day * diet, data = exp3_combined)

performance::check_model(exp3_combined_day_lm_4)
performance::check_model(exp3_combined_day_lm_4, check = c("qq"))
performance::check_model(exp3_combined_day_lm_4, check = c("linearity"))
performance::check_model(exp3_combined_day_lm_4, check = c("outliers"))

drop1(exp3_combined_day_lm_4, test = "F")

summary(exp3_combined_day_lm_4)

emmeans::emmeans(exp3_combined_day_lm_4, pairwise ~ day * diet)

# checking the model 
performance::check_model(exp3_combined_day_lm)
performance::check_model(exp3_combined_day_lm, check = c("qq"))
performance::check_model(exp3_combined_day_lm, check = c("linearity"))
# linearity looks not great

#-- making a gernealised linear model for day analysis 
exp3_combined_day_glm <- glm(fly_numbers ~ day * diet, family = poisson(link = "log"), data = exp3_combined)

# checking the new generalised model 2
performance::check_model(exp3_combined_day_glm)
performance::check_model(exp3_combined_day_glm, check = c("homogeneity", "qq"))

AIC(exp3_combined_day_lm_4, exp3_combined_day_glm)

# model not good 

# checking for overdispersion in the poisson generalised model
summary(exp3_combined_day_glm)

# model is overdispersed so using quasipoisson
exp3_combined_day_glm2 <- glm(fly_numbers ~ day, family = quasipoisson, data = exp3_combined)


# checking the new generalised model 2
performance::check_model(exp3_combined_day_glm2)
performance::check_model(exp3_combined_day_glm2, check = c("homogeneity", "qq"))
# homogenity looks more normal? 
# normality looks slightly worse but still okay - bad at the beginning
# stick with this exp3_combined_day_glm2

# do I need to do binomial instead of quasipoisson? 


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
performance::check_model(exp3_combined_lm, check = c("homogeneity"))
# looks ok? 
# go back and expand on this ?? maybe go beyond linear model

# trying out other models 
exp3_combined_glm <- glm(fly_numbers ~ diet, family = poisson, data = exp3_combined)

# using summary to look for overdispersion in the glm
summary(exp3_combined_glm)

# overdispersed so using quasipoisson
exp3_combined_glm_2 <- glm(fly_numbers ~ diet, family = quasipoisson, data = exp3_combined)

# checking the model
performance::check_model(exp3_combined_glm_2)
performance::check_model(exp3_combined_glm_2, check = c("qq"))
performance::check_model(exp3_combined_glm_2, check = c("homogeneity"))
# hard to compare homogeneity 

# but linear looks better for qq 

# trying other performance::check 

 model_performance(exp3_combined_lm)
 model_performance(exp3_combined_glm_2)
 
 compare_performance(exp3_combined_lm, exp3_combined_glm_2, verbose = FALSE )
 
 test_performance(exp3_combined_lm, exp3_combined_glm_2)
 
 
 # trying to do a MASS::boxcox
 MASS::boxcox(exp3_combined_lm)
 # error? 
 
 # trying linear in log and + 1 
 exp3_combined_lm_2 <- lm(formula = log(fly_numbers + 1) ~ diet, data = exp3_combined)
# stick with linear? 



# checking the model 
performance::check_model(exp3_combined_lm_2)
performance::check_model(exp3_combined_lm_2, check = c("qq"))
performance::check_model(exp3_combined_lm_2, check = c("linearity"))
performance::check_model(exp3_combined_lm_2, check = c("homogeneity"))
#  struggling to read this?
#  homogeneity defo not flat 
#  maybe stick with original linear model? 

# data analysis for chosen model

# using summary() which will show anova
summary(exp3_combined_lm)

# anova for chosen model 
anova(exp3_combined_lm)

# using emmeans tukey to test everything 
emmeans::emmeans(exp3_combined_lm, specs = pairwise ~ diet)


#--- TWO FACTOR FEEDING ANALYSIS --- FOOD CONDITIONS ----

# splitting up hard and soft diets and different nutrient diets 
exp3_combined$food_type <- ifelse(exp3_combined$diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
exp3_combined$food_nutrition <- ifelse(exp3_combined$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

# viewing the new dataset
view(exp3_combined)


# summarising hard vs soft data 
softhard_summary_exp3 <- exp3_combined %>%  
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
  geom_jitter(data = exp3_combined,
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
nutrient_summary_exp3 <- exp3_combined %>%  
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
  geom_jitter(data = exp3_combined,
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
softhard_plot_exp3 + nutrient_plot_exp3

#--- DATA ANALYSIS - TWO FACTOR FEEDING ANALYSIS --- FOOD CONDITIONS

# creating a linear model based on food nutrition and food type 
exp3_combined_foodcondition_lm <- lm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp3_combined)



exp3_combined_foodcondition_lm_2 <- lm(formula = log(fly_numbers + 1) ~ food_type * food_nutrition * day, data = exp3_combined)



# checking the model 
performance::check_model(exp3_combined_foodcondition_lm_2)
performance::check_model(exp3_combined_foodcondition_lm_2, check = c("normality", "qq"))
performance::check_model(exp3_combined_foodcondition_lm_2, check = c("linearity"))
performance::check_model(exp3_combined_foodcondition_lm_2, check = c("outliers"))

summary(exp3_combined_foodcondition_lm_2)

emmeans::emmeans(exp3_combined_foodcondition_lm_2, pairwise ~ food_type + food_nutrition + day + food_type:food_nutrition + food_type:day + day:food_nutrition)

# checking the model 
performance::check_model(exp3_combined_foodcondition_lm)
performance::check_model(exp3_combined_foodcondition_lm, check = c("normality", "qq"))
performance::check_model(exp3_combined_foodcondition_lm, check = c("linearity"))
# looks ok? 
# try a glm incase 
# why is there just one point for linearity/ homogenity?

# looking at a qq plot 
plot(exp3_combined_foodcondition_lm, which=c(1,3))
# understanding what this means? sorry

# variance inflation factor () 
car::vif(type = "predictor", exp3_combined_foodcondition_lm)
# no issues estimating the co-efficient? 

# transforming the data 
MASS::boxcox(exp3_combined_foodcondition_lm)
# Error in boxcox : response variable must be positive? 

#  cannot see whnat boxcox suggests but trying sqrt in the model to see if it changes 
exp3_combined_foodcondition_lm2 <- lm(sqrt(fly_numbers) ~ food_type  + food_nutrition + food_type : food_nutrition, data = exp3_combined)

# checking the new model 
performance::check_model(exp3_combined_foodcondition_lm2)
performance::check_model(exp3_combined_foodcondition_lm2, check = c("normality", "qq"))
performance::check_model(exp3_combined_foodcondition_lm2, check = c("linearity"))
# linearity has opposite problems 
# normality and qq looks better with sqrt model?
# BUT homogenity looks better with non sqrt model? 


# trying a glm model
exp3_combined_foodcondition_glm <- glm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = poisson, data = exp3_combined)

# trying summary function to look for overdispersion in poisson
summary(exp3_combined_foodcondition_glm)

# overdispersed so trying quasipoisson
exp3_combined_foodcondition_glm2 <- glm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = exp3_combined)


exp3_combined_foodcondition_glm_3 <- glm(formula = (fly_numbers + 1) ~ food_type + food_nutrition + day + food_type:food_nutrition + food_type:day + day:food_nutrition, family = poisson, data = exp3_combined)

performance::check_model(exp3_combined_foodcondition_glm_3)
performance::check_model(exp3_combined_foodcondition_glm_3, check = c("qq"))




# checking the glm 2 model 
performance::check_model(exp3_combined_foodcondition_glm2)
performance::check_model(exp3_combined_foodcondition_glm2, check = c("qq"))
# BEST TO CHECK THIS -- but looks okay? 
# struggling to understand what is better 

# still confused about what model to choose but maybe glm as not too sure about the points?


# still not understanding if these are good models so going a bit further with the analysis? 


# Use this for interaction effect? 
drop1(exp3_combined_foodcondition_glm2, test = "F")

#  using the chosen models for data analysis 
summary(exp3_combined_foodcondition_glm2)






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
       y = "Mean (+/- S.E.) number of eggs laid on each patch",
       title = "Overall Diets")+
  theme_classic()


#-- Exp 3 - egg - data analysis ----
#-- linear model of egg counting 
exp3_egg_lm <- lm(egg_numbers ~ diet, data = long_egg_counting3)

# checking the model
performance::check_model(exp3_egg_lm)
performance::check_model(exp3_egg_lm, check = c("qq"))
performance::check_model(exp3_egg_lm, check = c("linearity"))
# not a good model 

# trying a glm 
exp3_egg_glm <- glm(egg_numbers ~ diet, family = poisson, data = long_egg_counting3)

# using summary to look for glm overdispersion
summary(exp3_egg_glm)

# model is overdispersed so using quasipoisson
exp3_egg_glm2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting3)

# checking the model 
performance::check_model(exp3_egg_glm2)
performance::check_model(exp3_egg_glm2, check = c("qq"))
# qq looks better
#  homogenity looks ok 
# stick with this 

#  using the chosen model for data analysis 
# summary function to look at anova 
summary(exp3_egg_glm2)

# trying anova code 
anova(exp3_egg_glm2)

#-- analysing all egg data using tukey emmeans 
emmeans::emmeans(exp3_egg_glm2, specs = pairwise ~ diet)



#--------- TWO-FACTOR -- EGG COUNTING - EXPERIMENT 3 ----
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
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs on a patch",
       title = "")+
  theme_classic() 
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
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs on a patch",
       title = "")+
  theme_classic() 
# combining the experiment hardness plot with the nutrient plot with patchwork 
softhardegg_plot_exp3 + nutrientegg_plot_exp3


# TWO FACTOR ANALYSIS - OVIPOSITION PREFERENCE 
# TWO FACTOR ANALYSIS -EGG COUNTING - DATA ANALYSIS ----

# trying a linear model
exp3_egg_foodcondition_lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, data = long_egg_counting3)

# checking the model
performance::check_model(exp3_egg_foodcondition_lm)
performance::check_model(exp3_egg_foodcondition_lm, check = c("qq"))
performance::check_model(exp3_egg_foodcondition_lm, check = c("linearity"))
# neither the qq or the linearity model looks great 

# trying a glm
exp3_egg_foodcondition_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = poisson, data = long_egg_counting3)
summary(exp3_egg_foodcondition_glm)

exp3_egg_foodcondition_glm2 <- glm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = quasipoisson, data = long_egg_counting3)

# checking the model
performance::check_model(exp3_egg_foodcondition_glm2)
performance::check_model(exp3_egg_foodcondition_glm2, check = c("qq"))
# qq looks a lot better 
# homogenity looks similar but maybe better 
# go with this model? 

# significance of interaction effect 
drop1(exp3_egg_foodcondition_glm2, test = "F")

# data analysis for the chosen model 
summary(exp3_egg_foodcondition_glm2)



# interaction effect is not significant 
# trying new without interaction effect
exp3_egg_foodcondition_glm3 <- glm(egg_numbers ~ food_type + food_nutrition, family = quasipoisson, data = long_egg_counting3)
# looking for overdispersion in the new glm
summary(exp3_egg_foodcondition_glm3)
# glm is overdispersed so using quasipoisson
exp3_egg_foodcondition_glm4 <- glm(egg_numbers ~ food_type + food_nutrition, family = quasipoisson, data = long_egg_counting3)



# checking the new generalised linear model 
performance::check_model(exp3_egg_foodcondition_glm4)
performance::check_model(exp3_egg_foodcondition_glm4, check = c("qq"))
performance::check_model(exp3_egg_foodcondition_glm4, check = c("homogeneity"))
# qq looks okay
# homogeneity looks a bit slopey 

# trying a linear model 
exp3_egg_foodcondition_lm_2 <- lm(egg_numbers ~ food_type + food_nutrition, data = long_egg_counting3)

# checking the new linear model 
performance::check_model(exp3_egg_foodcondition_lm_2)
performance::check_model(exp3_egg_foodcondition_lm_2, check = c("qq"))
performance::check_model(exp3_egg_foodcondition_lm_2, check = c("homogeneity"))
# the generalised linear model looks a lot better 

# final choice is the NEW generalised linear model with quassipoisson
summary(exp3_egg_foodcondition_glm4)




