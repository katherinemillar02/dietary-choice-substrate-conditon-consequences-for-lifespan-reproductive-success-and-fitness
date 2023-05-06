# DATA ANALYSIS 
# SPLITTING UP FOOD HARDNESS AND NUTRIENT COMPOSITION 

# splitting up hard and soft diets and differernt nutrient diets 
exp2_combined$food_type <- ifelse(exp2_combined$diet %in% c("8:1H", "1:2H"), "Hard", "Soft")
exp2_combined$food_nutrition <- ifelse(exp2_combined$diet %in% c("8:1", "1:2H", "1:2S"), "1:2", "8:1")

# calculations for the food condition data 
softhard_summary_exp2 <- exp2_combined %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

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
# there are strong interaction effects 

# summary function which will show anova? 
summary(exp2_combined_foodcondition_lm)

# using anova function 
anova(exp2_combined_foodcondition_lm)


# confidence intervals
confint(exp2_combined_foodcondition_lm)

# table of data 
tbl_regression(exp2_combined_foodcondition_lm)



