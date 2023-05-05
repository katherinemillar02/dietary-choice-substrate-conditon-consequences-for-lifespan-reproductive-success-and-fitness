
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

# actual ANOVA 
anova(exp1b_combined_foodconditions_lm_5)