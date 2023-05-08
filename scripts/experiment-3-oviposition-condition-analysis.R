# changing the data to columns 
long_egg_counting3$food_type <- ifelse(long_egg_counting3 $diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
long_egg_counting3$food_nutrition <- ifelse(long_egg_counting3 $diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")

# code to view the new data
view(long_egg_counting3)

# Visualising the data soft and hard 
softhardegg_summary_exp3 <- long_egg_counting3 %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

# summarising egg nutrient composition 
nutrientegg_summary_exp3 <- long_egg_counting3%>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))

# trying a linear model
exp3_egg_foodcondition_lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, data = long_egg_counting3)

# checking the model
performance::check_model(exp3_egg_foodcondition_lm)
performance::check_model(exp3_egg_foodcondition_lm, check = c("qq"))
performance::check_model(exp3_egg_foodcondition_lm, check = c("linearity"))
# neither the qq or the linearity model looks great 

# trying a glm
exp3_egg_foodcondition_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = poisson, data = long_egg_counting3)
summary(exp3_egg_foodcondition_glm)

exp3_egg_foodcondition_glm2 <- glm(egg_numbers ~ food_type + food_nutrition + food_type * food_nutrition, family = quasipoisson, data = long_egg_counting3)

# checking the model
performance::check_model(exp3_egg_foodcondition_glm2)
performance::check_model(exp3_egg_foodcondition_glm2, check = c("qq"))
# qq looks a lot better 
# homogenity looks similar but maybe better 
# go with this model? 

# significance of interaction effect 
drop1(exp3_egg_foodcondition_glm2, test = "F")

# data analysis for the chosen model 
summary(exp3_egg_foodcondition_glm2)



# interaction effect is not significant 
# trying new without interaction effect
exp3_egg_foodcondition_glm3 <- glm(egg_numbers ~ food_type + food_nutrition, family = quasipoisson, data = long_egg_counting3)
# looking for overdispersion in the new glm
summary(exp3_egg_foodcondition_glm3)
# glm is overdispersed so using quasipoisson
exp3_egg_foodcondition_glm4 <- glm(egg_numbers ~ food_type + food_nutrition, family = quasipoisson, data = long_egg_counting3)



# checking the new generalised linear model 
performance::check_model(exp3_egg_foodcondition_glm4)
performance::check_model(exp3_egg_foodcondition_glm4, check = c("qq"))
performance::check_model(exp3_egg_foodcondition_glm4, check = c("homogeneity"))
# qq looks okay
# homogeneity looks a bit slopey 

# trying a linear model 
exp3_egg_foodcondition_lm_2 <- lm(egg_numbers ~ food_type + food_nutrition, data = long_egg_counting3)

# checking the new linear model 
performance::check_model(exp3_egg_foodcondition_lm_2)
performance::check_model(exp3_egg_foodcondition_lm_2, check = c("qq"))
performance::check_model(exp3_egg_foodcondition_lm_2, check = c("homogeneity"))
# the generalised linear model looks a lot better 

# final choice is the NEW generalised linear model with quassipoisson
summary(exp3_egg_foodcondition_glm4)
