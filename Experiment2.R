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


# -----------Feeding behaviour day 1 -------

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

#------------ Feeding behaviour day 2  ------
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


#------------ Combined days data analysis -----

#-- mutating a day variable

exp2d1 <- long_feedinge2d1 %>% mutate(day = "1")
exp2d2 <- long_feedinge2d2 %>% mutate(day = "2")


#- binding the data 
exp2both <- rbind(exp2d1, exp2d2)

#- exp 2 summary both days
exp2both_summary <- exp2both %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

#- visualising the data for exp2 both days 
exp2both_plot <- exp2both_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp2both,
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

#- using a linear model for feeding behaviour 
exp2bothlm <- lm(fly_numbers ~ diet + day, data = exp2both)

#- making a linear model with an interaction effect 
exp2bothlm2 <- lm(fly_numbers ~ diet + day + diet * day, data = exp2both)
#- maybe will not do an interaction effect for day, as not significant alone 
# and not part of the hypothesis


#-  testing the significance in day for both lm and glm 
drop1(exp2bothlm, test = "F")
drop1(exp2bothglm, test = "F")

drop1(exp2bothlm2, test = "F")
#-- day is not significant! yay well it is but only just 

#- using summary function for the linear model 
summary(exp2bothlm)

#-  making a glm 
exp2bothglm <- glm(fly_numbers ~ diet + day, family = poisson, data = exp2both)

#- making an lm 
exp2bothlm2 <- lm(fly_numbers ~ diet,  data = exp2both)

#- an interaction effect of diet in a linear model
exp2bothlm3 <- lm(fly_numbers ~ diet * diet,  data = exp2both)

#- summarising interaction effect linear model 
summary(exp2bothlm3)

#- looking for the significance of diet 
drop1(exp2bothlm3, test = "F")


#- using summary function for the general linear model 
summary(exp2bothglm)


#-- using quasipoisson to count for overdispersion in a glm 
exp2bothglm2 <- glm(fly_numbers ~ diet + day, family = quasipoisson, data = exp2both)

#- using summary function for the general linear model with quasipoisson
summary(exp2bothglm2)

#- using emmeans to test the linear model in experiment 2 (without day in the model)
emmeans::emmeans(exp2bothlm, specs = pairwise ~ diet)

#  anova of the linear model of both days
anova(exp2bothlm)
# one-way anova 
anova(exp2bothlm2)

# tidyverse version of a summary of data 
broom::tidy(exp2bothlm, conf.int = T)
broom::tidy(exp2bothlm2, conf.int = T)

# Using GGally to look at the dispersion of means 
GGally::ggcoef_model(exp2bothlm2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

# looking for interaction effects between diets 
exp2both %>% ggplot(aes(x=fly_numbers, y=diet, colour = diet, fill = diet, group = diet))+
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

# ------------ two factor data-analysis ------

# splitting up hard and soft diets and differernt nutrient diets 
exp2both$food_type <- ifelse(exp2both$diet %in% c("8:1H", "1:2H"), "hard", "soft")
exp2both$food_nutrition <- ifelse(exp2both$diet %in% c("8:1", "1:2H"), "1:2", "8:1")

# splitting the data into hard and soft groups and into nutrient groups 
hard_data <- subset(exp2both, food_type == "hard")
soft_data <- subset(exp2both, food_type == "soft")
eight_data <- subset(exp2both, food_nutrition == "8:1")
onetwo_data <- subset(exp2both, food_nutrition == "1:2")

# binding the split data together 
binded <- rbind(hard_data, soft_data, eight_data, onetwo_data)

# trying a two-way anova 
aov(fly_numbers ~ food_type + food_nutrition, data = binded)

# creating a linear model of fly numbers and food type and food nutrition 
bindedlm <- lm(fly_numbers ~ food_type + food_nutrition, data = binded)

# summarising the linear model data 
summary(bindedlm)

# trying a tukey test with emmeans - NOT WORKING 
emmeans::emmeans(binded, specs = fly_numbers ~ food_nutrition + food_type)

# summarising hard vs soft 
softhard_summary <- binded %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

# a soft vs hard plot 
softhard_plot <- softhard_summary %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = binded,
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
  theme_minimal() 




# summarising nutrient composition 
nutrient_summary <- binded %>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))




# a nutrient plot 
nutrient_plot <- nutrient_summary %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = binded,
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
  theme_minimal() 


# using patchwork to compare soft/hardness and nutrient composition 
softhard_plot + nutrient_plot




# egg counting data analysis 

# -------- (Exp 2) Egg counting  --------

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
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch")+
  theme_minimal()

#------- (Exp2) Egg counting data analysis ---------

#-- Making a linear model 
eggcountinge2ls1 <- lm(egg_numbers ~ diet, data = long_egg_counting2)
#---- Checking the model 
performance::check_model(eggcountinge2ls1)
#---- summarising the data 
summary(eggcountinge2ls1)

#-- emmeans to look for significance 
emmeans::emmeans(eggcountinge2ls1, specs = pairwise ~ diet)

#---- one-way anova
anova(eggcountinge2ls1) 


confint(eggcountinge2ls1)

# tidyverse summary
broom::tidy(eggcountinge2ls1,  
            exponentiate=T, 
            conf.int=)

GGally::ggcoef_model(eggcountinge2ls1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)








