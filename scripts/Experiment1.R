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

#-------------- (Exp 1a) Day 1 Data analysis  -----------

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

# (Exp1a) Day 2 Data analysis 
# Data analysis for just day 2 
#------- creating a linear model for day 1 
exp1lmd2 <- lm(fly_numbers ~ diet, data = long_feedinge1d2)
#------- using summary function for the model 
summary(exp1lmd2)
#-- Using emmeans to look for significant differences 
emmeans::emmeans(exp1lmd2, specs = pairwise ~ diet) 



#------- comparing the days using patchwork

exp1feeding_plotd1 + exp1feeding_plotd2


#----- (Exp1a) Combined days data -----

#------- Combining the data for feeding behaviour 

#------- Mutating a variable for day 
exp1d1 <- long_feedinge1d1 %>% mutate(day = "a1")
exp1d2 <- long_feedinge1d2 %>% mutate(day = "a2")
#------- Combining the days 
exp1all <- rbind(exp1d1, exp1d2)
# summarising the combined days data 
exp1all_summary <- exp1all %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

# visualising the data for combined days 
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


# ------ -(Exp1a) Combined days data analysis ------

# Testing a model for feeding behaviour for both days 
exp1alllm <- lm(fly_numbers ~ diet + day, data = exp1all)
# Using summary function for analysis 
summary(exp1alllm)
# using em means to test everything
emmeans::emmeans(exp1alllm, specs = pairwise ~ diet + day) 
# testing for significance in day 
drop1(exp1alllm, test = "F")




# -------- (Exp 1a) Egg counting  --------

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
       y = "Mean (+/- S.E.) number of eggs laid on each patch")+
  theme_minimal()

#------- (Exp1a) Egg counting data analysis -----

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



# ---------------- Experiment 1b - repeating the experiment -----

#----- (Exp1b) Day 1 -----
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


#-----  (Exp1b) data analysis -----


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


#- Data analysis of combined days (experiment 1b)

#------- Mutating a variable for day 
exp1bd1 <- long_feedinge1bd1 %>% mutate(day = "b1") 
exp1bd2 <- long_feedinge1bd2 %>% mutate(day = "b2")

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


# Testing a model for feeding behaviour for both days 
exp1balllm <- lm(fly_numbers ~ diet + day, data = exp1ball)
# Using summary function for analysis 
summary(exp1balllm)
# using em means to test everything
emmeans::emmeans(exp1balllm, specs = pairwise ~ diet + day) 
# testing for significance in day 
drop1(exp1balllm, test = "F")







# ------ experiment 1b

# (Exp1b) Egg count data analysis ------

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
performance::check_model(eggcountinge1bls1)
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

# Combining experiments data -----

# adding a variable for a and b 
exp1a <- exp1all %>% mutate(experiment = "exp1a") 
exp1b <- exp1ball %>% mutate(experiment = "exp1b")



#  testing the overall for experiment 1a against experiment 1b 

exp1both <- rbind(exp1a, exp1b)


# summarising the combined days data 
exp1both_summary <- exp1both %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

# visualising the data for combined days 
exp1both_plot <- exp1both_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp1both,
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


# Combining experiments data analysis -----


# Testing a model for feeding behaviour for both days 
exp1bothlm <- lm(fly_numbers ~ diet + experiment + day, data = exp1both)
# Using summary function for analysis 
summary(exp1bothlm)
# using em means to test everything
emmeans::emmeans(exp1bothlm, specs = pairwise ~ diet + experiment + day) 
# testing for significance in day 
drop1(exp1bothlm, test = "F")


#---- Combined experiments egg data ----
#------- collating egg counting data to look for significance 

#--- adding an experiment variable to egg counting data 
exp1aegg<- long_egg_counting1 %>% mutate(experiment = "exp1a")
exp1begg <- long_egg_counting1b %>% mutate(experiment = "exp1b")

#---  combining the data 
eggboth <- rbind(exp1aegg, exp1begg)

#---- Combined experiments egg data analysis ----

#---  linear model for collated egg counting data 
eggcountingboth <- lm(egg_numbers ~ diet + experiment, data = eggboth)


# experiment can probably be dropped from the model - 
drop1(eggcountingboth, test = "F" )

# analysing the data for egg counting 
summary(eggcountingboth)

emmeans::emmeans(eggcountingboth, specs = pairwise ~ diet) 

# comparing the repeats of experiment 1a
# using patchwork to compare experiment 1a and experiment 1b 

exp1all_plot + exp1ball_plot

# test everything individually even the individual days? 



# an emmeans model without day and experiment in the model - to look for significance between 1:2 S and H 


emmeans::emmeans(exp1bothlm, specs = pairwise ~ diet) 

# plot of all combined egg data 

eggboth_summary <- eggboth %>%  
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))


egg_counting_plot_all <- eggboth_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = eggboth,
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


