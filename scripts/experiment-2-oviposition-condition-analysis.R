


# splitting up the count data into two sections of food hardness and food nutrition
long_egg_counting2$food_type <- ifelse(long_egg_counting2 $diet %in% c("8:1H", "1:2H"), "Hard", "Soft")
long_egg_counting2$food_nutrition <- ifelse(long_egg_counting2 $diet %in% c("8:1", "1:2H", "1:2S"), "1:2", "8:1")

# calculations of the egg count data of food hardness 
softhardegg_summary <- long_egg_counting2 %>%  
  group_by(food_type) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))


# calculations of the egg count data of nutrient composition 
nutrientegg_summary <- long_egg_counting2%>%  
  group_by(food_nutrition) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))



# doing a linear model of egg numbers with interaction effects  
exp2_egg_foodcondition_lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = long_egg_counting2)

# Checking the performance of the model 
performance::check_model(exp2_egg_foodcondition_lm)
performance::check_model(exp2_egg_foodcondition_lm, check = c("qq"))
performance::check_model(exp2_egg_foodcondition_lm, check = c("linearity"))
# looks quite bad 

# trying a glm 
exp2_egg_foodcondition_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = poisson, data = long_egg_counting2)

# looking for overdispersion
summary(exp2_egg_foodcondition_glm)

# overdispersed so using quasipoisson
exp2_egg_foodcondition_glm2 <- glm(egg_numbers ~ food_type + food_nutrition, family = quasipoisson, data = long_egg_counting2)

#---- Checking the model 
performance::check_model(exp2_egg_foodcondition_glm2)
performance::check_model(exp2_egg_foodcondition_glm2, check = c("qq"))
# homogenity still looks slopey
# normality looks a lot better
# stick with this model so far