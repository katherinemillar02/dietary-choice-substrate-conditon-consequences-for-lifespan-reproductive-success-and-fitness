# (Exp3) Day 1 
#-- Reading the data in
feedinge3d1 <- read_excel("data/RPFemaleFeedingE3D1.xlsx")
#--- Making the data long
long_feedinge3d1 <- feedinge3d1 %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "diet", values_to = "fly_numbers")
# summary of just day 2 
exp3feeding_summary_d1 <- long_feedinge3d1 %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))
# (Exp3) Day 2 
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


#--- mutating a day variable to combine the data 
exp3d1 <- long_feedinge3d1 %>% mutate(day = "1")
exp3d2 <- long_feedinge3d2 %>% mutate(day = "2")
#--- combining the two days 
exp3_combined <- rbind(exp3d1, exp3d2)



#--- Data analysis for combined days data ----

# playing around 
# removing an accidental bit of data 
exp3_combined <- exp3_combined %>% filter(fly_numbers <60)



#-- making a linear model for day analysis 
exp3_combined_day_lm_2 <- lm(fly_numbers ~ day * diet, data = exp3_combined)

# checking the model 
performance::check_model(exp3_combined_day_lm_2)
performance::check_model(exp3_combined_day_lm_2, check = c("qq"))
performance::check_model(exp3_combined_day_lm_2, check = c("linearity"))
performance::check_model(exp3_combined_day_lm_2, check = c("outliers"))
# cooks distance = 0.9 - 
# slope - something hasn't been measured 
# more error at high than low values

# adding fly numbers +1 so a MASSBOX can be used 
# some of the values are 0 
exp3_combined_day_lm_3 <- lm(formula = (fly_numbers + 1) ~ day * diet, data = exp3_combined)

# using a mass box cox to see what the data should be in 
MASS::boxcox(exp3_combined_day_lm_3)

# putting the formula in log 
exp3_combined_day_lm_4 <- lm(formula = log(fly_numbers + 1) ~ day * diet, data = exp3_combined)

# performance check of the new linear model
performance::check_model(exp3_combined_day_lm_4)
performance::check_model(exp3_combined_day_lm_4, check = c("qq"))
performance::check_model(exp3_combined_day_lm_4, check = c("linearity"))
performance::check_model(exp3_combined_day_lm_4, check = c("outliers"))
# performance looks okay

# testing for significance between day and diet 
drop1(exp3_combined_day_lm_4, test = "F")
# strong statistical significance 

# using summary function to compare values 
summary(exp3_combined_day_lm_4)

# using emmeans to compare values 
emmeans::emmeans(exp3_combined_day_lm_4, pairwise ~ day * diet)

# trying a glm 

#-- making a gernealised linear model for day analysis 
exp3_combined_day_glm <- glm(fly_numbers ~ day * diet, family = poisson(link = "log"), data = exp3_combined)

# checking the new generalised model 2
performance::check_model(exp3_combined_day_glm)
performance::check_model(exp3_combined_day_glm, check = c("homogeneity", "qq"))

# comparing the models 
AIC(exp3_combined_day_lm_4, exp3_combined_day_glm)
