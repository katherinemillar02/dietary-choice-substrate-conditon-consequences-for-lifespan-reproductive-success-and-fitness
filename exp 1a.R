exp1a_all_day_lm_2 <- lm(formula = log(fly_numbers + 1) ~ day * diet , data = exp1a_all)

summary(exp1a_all_day_lm_2)

exp1a_all_day_lm_3 <- lm(formula = log(fly_numbers + 1) ~  diet , data = exp1a_all)

summary(exp1a_all_day_lm_3)

emmeans::emmeans(exp1a_all_day_lm_3, pairwise ~ diet)


exp1a_all


exp1a_all$food_type <- ifelse(exp1a_all$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
exp1a_all$food_nutrition <- ifelse(exp1a_all$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")

exp1_combined_foodconditions_lm <- lm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1a_all)

# checking the new  linear model 
performance::check_model(exp1_combined_foodconditions_lm)
performance::check_model(exp1_combined_foodconditions_lm, check = c("qq"))
performance::check_model(exp1_combined_foodconditions_lm, check = c("homogeneity"))


exp1_combined_foodconditions_lm_2 <- lm(formula = log(fly_numbers + 1) ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1a_all)

performance::check_model(exp1_combined_foodconditions_lm_2)
performance::check_model(exp1_combined_foodconditions_lm_2, check = c("qq"))
performance::check_model(exp1_combined_foodconditions_lm_2, check = c("homogeneity"))

MASS::boxcox(exp1_combined_foodconditions_lm_2 )

summary(exp1_combined_foodconditions_lm_2)


exp1_combined_foodconditions_glm <- glm((fly_numbers +1) ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson(link = "log"), data = exp1a_all)

performance::check_model(exp1_combined_foodconditions_glm)
performance::check_model(exp1_combined_foodconditions_glm, check = c("qq"))
performance::check_model(exp1_combined_foodconditions_glm, check = c("outliers"))
performance::check_model(exp1_combined_foodconditions_glm, check = c("homogeneity"))




