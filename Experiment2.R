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


#-- mutating a day variable

exp2d1 <- long_feedinge2d1 %>% mutate(day = "1")
exp2d2 <- long_feedinge2d2 %>% mutate(day = "2")

exp2both <- rbind(exp2d1, exp2d2)

exp2bothlm <- lm(fly_numbers ~ diet + day, data = exp2both)

drop1(exp2bothlm, test = "F")

summary(exp2bothlm)

#-- day is not significant! yay 

emmeans::emmeans(exp2bothlm, specs = pairwise ~ diet)


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

#------- (Exp1a) Egg counting data analysis ---------

#-- Making a linear model 
eggcountinge2ls1 <- lm(egg_numbers ~ diet, data = long_egg_counting2)
#---- Checking the model 
performance::check_model(eggcountinge2ls1)
#---- summarising the data 
summary(eggcountinge2ls1)

emmeans::emmeans(eggcountinge2ls1, specs = pairwise ~ diet)
#----  doing tests 
anova(eggcountinge2ls1) 
confint(eggcountinge2ls1)
# tidyverse summary
broom::tidy(eggcountinge2ls1,  
            exponentiate=T, 
            conf.int=T)









