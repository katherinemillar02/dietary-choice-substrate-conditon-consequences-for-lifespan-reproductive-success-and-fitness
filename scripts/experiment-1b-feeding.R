# NEW SCRIPT FOR EXPERIMENT 1B
# MADE TO LOOK AT DAYS SEPERATLEY 


# Data analysis 

# Trying a linear model in log and dependant variable + 1 
exp1b_all_day_lm_3 <- lm(formula = log(fly_numbers + 1) ~  diet * day , data = exp1ball)

# Doing performance-checks for lm_3
performance::check_model(exp1b_all_day_lm_3)
performance::check_model(exp1b_all_day_lm_3, check = c("qq"))
performance::check_model(exp1b_all_day_lm_3, check = c("homogeneity"))
performance::check_model(exp1b_all_day_lm_3, check = c("linearity"))
performance::check_model(exp1b_all_day_lm_3, check = c("outliers"))

# Model looks okay 

# Trying a generalised linear model in log and dependant variable + 1 
# Added sqrt frimn MASS:: box cox analysis 
exp1b_all_day_glm_3 <- glm(formula = sqrt(fly_numbers + 1) ~  diet * day, family = quasipoisson(link = "log"), data = exp1ball)

# Checking the performance for the generalised linear model_3 
performance::check_model(exp1b_all_day_glm_3)
performance::check_model(exp1b_all_day_glm_3, check = c("qq"))
performance::check_model(exp1b_all_day_glm_3, check = c("homogeneity"))
# use this model 
# performance check looks good 

# testing for significance in day using drop1
drop1(exp1b_all_day_glm_3,test = "F")
# Result will show F statistic 

# Using summary function to do another analysis 
summary(exp1b_all_day_glm_3)

# Using emmeans to get analysis of everything and tukey
emmeans::emmeans(exp1b_all_day_glm_3, pairwise ~ diet * day)


# TWO FACTOR ANALYSIS 
# Seperating the data for hard and soft and 1:8 and 1:2 into seperate variables
exp1ball$food_type <- ifelse(exp1ball$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
exp1ball$food_nutrition <- ifelse(exp1ball$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")

# Doing a linear model on the factors with interaction effects 
exp1b_combined_foodconditions_lm <- lm(fly_numbers ~ food_type * food_nutrition * day, data = exp1ball)

# Doing performance checks of this linear model
performance::check_model(exp1b_combined_foodconditions_lm )
performance::check_model(exp1b_combined_foodconditions_lm, check = c("qq"))
performance::check_model(exp1b_combined_foodconditions_lm, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_lm, check = c("linearity")) 
performance::check_model(exp1b_combined_foodconditions_lm, check = c("outliers"))
# qq doesn't look too good 
# homogeneity looks alright
# linearity is a bit slopey 

# Trying a generalised linear model with interaction effects 
# with quasipoisson to account for overdispersion
exp1b_combined_foodconditions_glm <- glm(fly_numbers ~ food_type * food_nutrition * day, family=quasipoisson, data = exp1ball)

# Doing a performance check of the model 
performance::check_model(exp1b_combined_foodconditions_glm)
performance::check_model(exp1b_combined_foodconditions_glm, check = c("qq"))
performance::check_model(exp1b_combined_foodconditions_glm, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_glm, check = c("linearity"))
performance::check_model(exp1b_combined_foodconditions_glm, check = c("outliers"))
# qq doesnt look too good 
# homogeneity looks ok 

# trying a linear model again 
# model has + 1 so a MASS boxcox could be tested 
exp1b_combined_foodconditions_lm_2 <- lm(fly_numbers + 1 ~ food_type * food_nutrition * day, data = exp1ball) 

#  MASS BOX COX to see if model needs to be transformed 
MASS::boxcox(exp1b_combined_foodconditions_lm_2)
# BOXCOX shows model could be logged 

# Putting fly numbers in log and _ 1 
exp1b_combined_foodconditions_lm_3 <- lm(formula = log(fly_numbers + 1) ~ food_type * food_nutrition * day, data = exp1ball)

# Performance-checking the model 
performance::check_model(exp1b_combined_foodconditions_lm_3)
performance::check_model(exp1b_combined_foodconditions_lm_3, check = c("qq"))
performance::check_model(exp1b_combined_foodconditions_lm_3, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_lm_3, check = c("linearity"))
performance::check_model(exp1b_combined_foodconditions_lm_3, check = c("outliers"))
# qq looks good 
# homogeneity looks good 
# linearity is a bit slopey 

# Looking for an interaction between all the factors 
drop1(exp1b_combined_foodconditions_lm_3, test = "F")
#  best to not include day as an inueraction effect 


# taking day out as an interaction
exp1b_combined_foodconditions_lm_4 <- lm(formula = log(fly_numbers + 1) ~ food_type * food_nutrition + day, data = exp1ball)

# doing a performance check if this new chosen model 
performance::check_model(exp1b_combined_foodconditions_lm_4)
performance::check_model(exp1b_combined_foodconditions_lm_4, check = c("qq"))
performance::check_model(exp1b_combined_foodconditions_lm_4, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_lm_4, check = c("linearity"))
performance::check_model(exp1b_combined_foodconditions_lm_4, check = c("outliers"))
# model looks okay 

# using summary function to get anova analysis 
summary(exp1b_combined_foodconditions_lm_4)

# dropping the interaction effect of food nutrition and food hardness 
exp1b_combined_foodconditions_lm_5 <- lm(formula = log(fly_numbers + 1) ~ food_type + food_nutrition + day, data = exp1ball)

# ANOVA analysis using summary function
summary(exp1b_combined_foodconditions_lm_5)

# Visualizing the data 

# using boxplots to look at the different variables

# comparing soft and hard foods 
foodhardness_boxplot_exp1b_feeding <- ggplot()+ 
  geom_boxplot(exp1ball, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Substrate condition of diet",
       y = "Flies per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1ball,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# comparing 1:2 and 1:8 diets
nutrientcompositon_boxplot_exp1b_feeding <- ggplot()+ 
  geom_boxplot(exp1ball, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("#CF9FFF", "#FF8C69"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Flies per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1ball,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

# using patchwork to compare the plots together 
foodhardness_boxplot_exp1b_feeding + nutrientcompositon_boxplot_exp1b_feeding





