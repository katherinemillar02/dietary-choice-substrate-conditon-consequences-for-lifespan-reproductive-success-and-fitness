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


# -----------Feeding behaviour JUST day 1 -------
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

#------------ Feeding behaviour JUST day 2  ------
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

#  visualising the data for experiment 2 day 2 
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

#--  using patchwork to combine the plots for day 1 and day 2 
exp2feeding_plot_d1 + exp2feeding_plotd2


#------------ Combined days ----------- USE FROM HERE  ----
#------------ Combined days -- data visualisation -----
#---- visualising the data
#-- mutating a day variable
exp2d1 <- long_feedinge2d1 %>% mutate(day = "1")
exp2d2 <- long_feedinge2d2 %>% mutate(day = "2")
#- binding the data 
exp2_combined <- rbind(exp2d1, exp2d2)
#- exp 2 summary both days
exp2_combined_summary <- exp2_combined %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#  Table of the combined days data for experiment 2
exp2table <- exp2_combined %>% 
  group_by('Diet' = diet) %>% 
  summarise(`Mean flies on a diet`= mean(fly_numbers, na.rm = T),
            `SD`= sd(fly_numbers, na.rm = T)) %>% gt::gt()



#- visualising the data for exp2 both days 
exp2_combined_plot <- exp2_combined_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp2_combined,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "Overall Diets")+
  theme_classic() 




#------------ Combined days -- data analysis  -----

# testing for the significance in day
exp2_combined_days_lm <- lm(fly_numbers ~ day, data = exp2_combined)

# checking the model
performance::check_model(exp2_combined_days_lm)
performance::check_model(exp2_combined_days_lm, check = c("qq"))
performance::check_model(exp2_combined_days_lm, check = c("linearity"))
# data doesn't look too great


# trying a glm 
exp2_combined_days_glm <- glm(fly_numbers ~ day, family = poisson, data = exp2_combined)

# using summary function to look for overdispersion
summary(exp2_combined_days_glm)
# overdispersed 

# trying a glm  with quasipoisson
exp2_combined_days_glm_2 <- glm(fly_numbers ~ day * diet, family = quasipoisson, data = exp2_combined)


# checking the model
performance::check_model(exp2_combined_days_glm_2)
performance::check_model(exp2_combined_days_glm_2, check = c("qq"))
# still doesn't look great but homogenity looks better 

# trying a glm + 1 
exp2_combined_days_glm_3 <- glm(formula = (fly_numbers + 1) ~ day * diet, family = quasipoisson, data = exp2_combined)

# checking the model
performance::check_model(exp2_combined_days_glm_3)
performance::check_model(exp2_combined_days_glm_3, check = c("qq"))
performance::check_model(exp2_combined_days_glm_3, check = c("outliers"))
performance::check_model(exp2_combined_days_glm_3, check = c("homogeneity"))
# normality still looks bad at the beginning 
# ways to fix this? 

# from this the best model is exp2_combined_days_glm_3



# looking for significance in day
drop1(exp2_combined_days_glm_3, test = "F")

# summary shows completley different results? 
summary(exp2_combined_days_glm_3)

# analysing fly numbers and diet data 
# trying a linear model
exp2_combined_lm <- lm(fly_numbers ~ diet, data = exp2_combined)

# checking the model
performance::check_model(exp2_combined_lm)
performance::check_model(exp2_combined_lm, check = c("qq"))
performance::check_model(exp2_combined_lm, check = c("linearity"))
# qq looks sort of okay but linearity very uneven

# adding +1 to fly numbers as some are 0? 
exp2_combined_lm2 <- lm(formula = (fly_numbers + 1) ~ diet, data = exp2_combined)

summary(exp2_combined_lm2)

# checking the model 2 
performance::check_model(exp2_combined_lm2)
performance::check_model(exp2_combined_lm2, check = c("qq"))
performance::check_model(exp2_combined_lm2, check = c("linearity"))
# the + 1 doesn't make a difference? 

# trying a glm 
exp2_combined_glm <- glm(fly_numbers ~ diet, family = poisson, data = exp2_combined)

# using summary to look for overdispersion in the model
summary(exp2_combined_glm)

# model is overdispersed so doing quasipoisson
exp2_combined_glm2 <- glm(fly_numbers ~ diet, family = quasipoisson, data = exp2_combined)

# checking the new glm model 2 
performance::check_model(exp2_combined_glm2)
performance::check_model(exp2_combined_glm2, check = c("qq"))
# doesn't look any better

# from this exp2_combined_lm2 is the best model but linearity dodgy on both

# data analysis with the chosen model 
summary(exp2_combined_lm2)

#- using emmeans to test the linear model in experiment 2 (without day in the model)
emmeans::emmeans(exp2_combined_lm2, specs = pairwise ~ diet)



# tidyverse version of a summary of data 
broom::tidy(exp2_combined_lm2, conf.int = T)


# Using GGally to look at the dispersion of means 
GGally::ggcoef_model(exp2_combined_lm2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

# looking for interaction effects between diets 
exp2_combined %>% ggplot(aes(x=fly_numbers, y=diet, colour = diet, fill = diet, group = diet))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )


# ------------ Two factor/ food condition analysis / feeding ----

# splitting up hard and soft diets and differernt nutrient diets 
exp2_combined$food_type <- ifelse(exp2_combined$diet %in% c("8:1H", "1:2H"), "Hard", "Soft")
exp2_combined$food_nutrition <- ifelse(exp2_combined$diet %in% c("8:1", "1:2H", "1:2S"), "1:2", "8:1")
# viewing the new dataset
view(exp2_combined)

# Two factor/ food condition analysis / data visualisation ----

# summarising hard vs soft data 
softhard_summary_exp2 <- exp2_combined %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# a soft vs hard plot 
softhard_plot_exp2 <- softhard_summary_exp2 %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp2_combined,
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
nutrient_summary_exp2 <- exp2_combined %>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# a nutrient plot 
nutrient_plot_exp2 <- nutrient_summary_exp2 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp2_combined,
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
softhard_plot_exp2 + nutrient_plot_exp2

# ------------  Two factor/ food condition analysis / data analysis ----

# creating a linear model based on food nutrition and food type 
exp2_combined_foodcondition_lm <- lm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp2_combined)

# checking the model 
performance::check_model(exp2_combined_foodcondition_lm)
performance::check_model(exp2_combined_foodcondition_lm, check = c("qq"))
performance::check_model(exp2_combined_foodcondition_lm, check = c("linearity"))
# qq looks okay but linearity not good
# trying a glm
exp2_combined_foodcondition_glm <- glm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = poisson, data = exp2_combined)

# using summary to look for overdispersion
summary(exp2_combined_foodcondition_glm)

# overdispersed so adding quasipoisson 
exp2_combined_foodcondition_glm2 <- glm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = exp2_combined)


# checking the model 
performance::check_model(exp2_combined_foodcondition_glm2)
performance::check_model(exp2_combined_foodcondition_glm2, check = c("qq"))
# qq and homogenity look worse than the lm 

# stick with exp2_combined_foodcondition_lm

# doing data analysis with the chosen model

# using drop1 to look for significance of interaction effect 
drop1(exp2_combined_foodcondition_lm, test = "F")


# summary function which will show anova 
summary(exp2_combined_foodcondition_lm)


# confidence intervals
confint(exp2_combined_foodcondition_lm)

# table of data 
tbl_regression(exp2_combined_foodcondition_lm)




# splitting the data into hard and soft groups and into nutrient groups 
#hard_data <- subset(exp2both, food_type == "hard")
#soft_data <- subset(exp2both, food_type == "soft")
#eight_data <- subset(exp2both, food_nutrition == "8:1")
#onetwo_data <- subset(exp2both, food_nutrition == "1:2")

# binding the split data together 
#typestogether <- rbind(hard_data, soft_data, eight_data, onetwo_data)


# trying a two-way anova 
#aov(fly_numbers ~ food_type + food_nutrition, data = typestogether)

# creating a linear model of fly numbers and food type and food nutrition 
#typestogetherlm <- lm(fly_numbers ~ food_type + food_nutrition, data = typestogether)

#typestogetherlm2 <- lm(fly_numbers ~ food_type + food_nutrition + food_type * food_nutrition, data = typestogether)

# summarising the linear model data 
#summary(typestogetherlm)
#summary(typestogetherlm2)

# trying a tukey test with emmeans - NOT WORKING 
#emmeans::emmeans(typestogetherlm, specs = pairwise ~ food_nutrition + food_type)


# -------- OVIPOSITION PREFERENCE  --------

#____ Reading the data in 
egg_counting_data_e2 <- (read_excel(path = "data/RPEggCountE2.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting2 <- egg_counting_data_e2 %>% 
  pivot_longer(cols = ("8:1S":"1:2H"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting2_summary <- long_egg_counting2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
egg_counting2_plot <- egg_counting2_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting2,
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

#------- Ovipoistion Preference -- data analysis ---------
#-- Making a linear model 
exp2_egg_lm <- lm(egg_numbers ~ diet, data = long_egg_counting2)
#---- Checking the model 
performance::check_model(exp2_egg_lm)
performance::check_model(exp2_egg_lm, check = c("qq"))
performance::check_model(exp2_egg_lm, check = c("linearity"))
# model doesn't look very good

#-- Making a generalised linear model 
exp2_egg_glm <- glm(egg_numbers ~ diet, family = poisson, data = long_egg_counting2)

# using summary() to check for overdispersion
summary(exp2_egg_glm)

# using quasipoission as is overdispersed 
exp2_egg_glm2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting2)

#---- Checking the model 
performance::check_model(exp2_egg_glm2)
performance::check_model(exp2_egg_glm2, check = c("qq"))
# homogenity looks ok but still a alight slope
# go with this 

# doing data analysis for the chosen model 
summary(exp2_egg_glm2)

#-- emmeans tukey to look for significance of everything 
emmeans::emmeans(exp2_egg_glm2, specs = pairwise ~ diet)

#---- one-way anova? 
anova(exp2_egg_glm2) 

#-- looking at the confidence intervals 
confint(exp2_egg_glm2)

# tidyverse summary
broom::tidy(exp2_egg_glm2,  
            exponentiate=T, 
            conf.int=)

# visualising the means
GGally::ggcoef_model(exp2_egg_glm2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)



# TWO FACTOR ANALYSIS / food condition / OVIPOSITION PREFERENCE ----
# changing the data to columns 
long_egg_counting2$food_type <- ifelse(long_egg_counting2 $diet %in% c("8:1H", "1:2H"), "Hard", "Soft")
long_egg_counting2$food_nutrition <- ifelse(long_egg_counting2 $diet %in% c("8:1", "1:2H", "1:2S"), "1:2", "8:1")

# looking at the data 
view(long_egg_counting2)

# TWO FACTOR ANALYSIS / food condition / OVIPOSITION PREFERENCE / Data visualisation  ----
#- Making a summary of egg counting// soft and hard 
softhardegg_summary <- long_egg_counting2 %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#- Making a plot of egg counting// soft and hard 
softhardegg_plot <- softhardegg_summary %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting2,
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
nutrientegg_summary <- long_egg_counting2%>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
# a nutrient plot 
nutrientegg_plot <- nutrientegg_summary %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting2,
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

#- visualising the data of egg counting nutrient composition vs soft/hard together
softhardegg_plot + nutrientegg_plot


#- TWO FACTOR // OVIPOSITION // DATA ANALYSIS ------
# doing a linear model of egg two factor 
exp2_egg_foodcondition_lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = long_egg_counting2)
summary(exp2_egg_foodcondition_lm)
drop1(exp2_egg_foodcondition_lm, test = "F")
#---- Checking the model 
performance::check_model(exp2_egg_foodcondition_lm)
performance::check_model(exp2_egg_foodcondition_lm, check = c("qq"))
performance::check_model(exp2_egg_foodcondition_lm, check = c("linearity"))
# looks quite bad 

# trying a glm 
exp2_egg_foodcondition_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = poisson, data = long_egg_counting2)

# looking for overdispersion
summary(exp2_egg_foodcondition_glm)

# overdispersed so using quasipoisson
exp2_egg_foodcondition_glm2 <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = long_egg_counting2)

#---- Checking the model 
performance::check_model(exp2_egg_foodcondition_glm2)
performance::check_model(exp2_egg_foodcondition_glm2, check = c("qq"))
# homogenity still looks slopey
# normality looks a lot better
# stick with this model so far

# doing data analysis with chosen model 

# drop1 function to look for signifiance
drop1(exp2_egg_foodcondition_glm2, test = "F")
# interaction effect is not significant 

# summary function to get analysis 
summary(exp2_egg_foodcondition_glm2)

# anova for F values?
anova(exp2_egg_foodcondition_glm2)

# model without an interaction effect
# trying generalised linear model
exp2_egg_foodcondition_glm3 <- glm(egg_numbers ~ food_type + food_nutrition, family = poisson, data = long_egg_counting2)

# looking for overdisperion in new glm model 
summary(exp2_egg_foodcondition_glm3)

# overdispersed so using quasipoisson
exp2_egg_foodcondition_glm4 <- glm(egg_numbers ~ food_type + food_nutrition, family = quasipoisson, data = long_egg_counting2)

#---- Checking the new glm model 
performance::check_model(exp2_egg_foodcondition_glm4)
performance::check_model(exp2_egg_foodcondition_glm4, check = c("qq"))
performance::check_model(exp2_egg_foodcondition_glm4, check = c("homogeneity"))
# qq looks ok
# homogeneity could be better? 

# trying a linear model without interaction effect 
exp2_egg_foodcondition_lm2 <- lm(egg_numbers ~ food_type + food_nutrition, data = long_egg_counting2)

# checking the new lm model 
performance::check_model(exp2_egg_foodcondition_lm2)
performance::check_model(exp2_egg_foodcondition_lm2, check = c("qq"))
performance::check_model(exp2_egg_foodcondition_lm2, check = c("linearity"))
performance::check_model(exp2_egg_foodcondition_lm2, check = c("homogeneity"))
# homogeneity looks better on glm 4 ? 
# stick with glm 4 for now 
# this isn't as good 

# getting analysis of new mnodel with summary function
summary(exp2_egg_foodcondition_glm4)









# visualising the data for egg analysis 

#----###
# same thing but doing the subset function 
# probably do subset analysis if you want to analyse stuff previously 
#hard_data_egg <- subset(long_egg_counting2, food_type == "hard")
#soft_data_egg <- subset(long_egg_counting2, food_type == "soft")
#eight_data_egg <- subset(long_egg_counting2, food_nutrition == "8:1")
#onetwo_data_egg <- subset(long_egg_counting2, food_nutrition == "1:2")
#-eggexp2_new <- rbind(hard_data_egg, soft_data_egg, eight_data_egg, onetwo_data_egg)
#-eggexp2lm_new <-  lm(egg_numbers ~ food_type + food_nutrition, data = eggexp2_new)
#--summary(eggexp2lm_new)

