# splitting up hard and soft diets and different nutrient diets 
exp3_combined$food_type <- ifelse(exp3_combined$diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
exp3_combined$food_nutrition <- ifelse(exp3_combined$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

# summarising hard vs soft data 
softhard_summary_exp3 <- exp3_combined %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

# summarising nutrient composition data 
nutrient_summary_exp3 <- exp3_combined %>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))


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


