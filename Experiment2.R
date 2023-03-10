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

exp2bothlm2 <- lm(fly_numbers ~ diet,  data = exp2both)


exp2bothlm3 <- lm(fly_numbers ~ diet * diet,  data = exp2both)

summary(exp2bothlm3)

drop1(exp2bothlm3, test = "F")


#- using summary function for the general linear model 
summary(exp2bothglm)


#-- using quasipoisson to count for overdispersion
exp2bothglm2 <- glm(fly_numbers ~ diet + day, family = quasipoisson, data = exp2both)

#- using summary function for the general linear model with quasipoisson
summary(exp2bothglm2)

#- using emmeans to test the linear model in experiment 2 (without day in the model)
emmeans::emmeans(exp2bothlm, specs = pairwise ~ diet)

# two-way anova 

anova(exp2bothlm)

# one-way anova 


anova(exp2bothlm2)

broom::tidy(exp2bothlm, conf.int = T)
broom::tidy(exp2bothlm2, conf.int = T)

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

# -------- 
# Load your data into R


# Create a new column to indicate hard vs soft diets
exp2both$food_type <- ifelse(exp2both$diet %in% c("8:1H", "1:2H"), "hard", "soft")

# Split the data into hard and soft groups
hard_data <- subset(exp2both, food_type == "hard")
soft_data <- subset(exp2both, food_type == "soft")


binded <- rbind(hard_data, soft_data)

bindedlm <- lm(fly_numbers ~ food_type, data = binded)

summary(bindedlm)



aov(fly_numbers ~ food_type, data = binded)

softhard_summary <- binded %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


softhard_plot <- softhard_summary %>% 
  ggplot(aes(x = diet, y = mean))+
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




exp2both$food_type <- ifelse(exp2both$diet == "trt1" | exp2both$diet == "trt2", "soft", "hard")

model <- aov(fly_numbers ~ diet + food_type, data = exp2both)

summary(model)

section1 <- subset(exp3both, diet == "8:1H" & diet == "1:2H")

section2 <- subset(exp3both, diet == "8:1S" & diet == "1:2S")


hardness1 <- subset(exp3both, diet == "8:1H" | diet == "8:1S")
hardness2 <- subset(exp3both, diet == "1:2H" | diet == "1:2S")

summary(hardness1$fly_numbers, hardness2$fly_numbers)

fh <- rbind(hardness1, hardness2)

summary(fh)

fhlm <- lm(fly_numbers + hardness1 + hardness2, data = fh)


model1 <- aov(y ~ treat + time, data = section1)

model2 <- aov(y ~ treat + time, data = section2)


model3 <- aov(fly_numbers ~ diet + diet, data = section1)

model4 <- aov(y ~ treat + time, data = section4)


summary(model3)

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
            conf.int=T)

GGally::ggcoef_model(eggcountinge2ls1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)









