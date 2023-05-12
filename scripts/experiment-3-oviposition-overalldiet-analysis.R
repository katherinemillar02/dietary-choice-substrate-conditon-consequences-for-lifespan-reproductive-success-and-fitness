#____ Reading the data in 
egg_counting_data_e3 <- (read_excel(path = "data/RPEggCountE3.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting3 <- egg_counting_data_e3 %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting3_summary <- long_egg_counting3 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))


#-- Exp 3 - egg - data analysis ----
#-- linear model of egg counting 
exp3_egg_lm <- lm(egg_numbers ~ diet, data = long_egg_counting3)

# checking the model
performance::check_model(exp3_egg_lm)
performance::check_model(exp3_egg_lm, check = c("qq"))
performance::check_model(exp3_egg_lm, check = c("linearity"))
# not a good model 

# trying a glm 
exp3_egg_glm <- glm(egg_numbers ~ diet, family = poisson, data = long_egg_counting3)

# using summary to look for glm overdispersion
summary(exp3_egg_glm)

# model is overdispersed so using quasipoisson
exp3_egg_glm2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting3)

# checking the model 
performance::check_model(exp3_egg_glm2)
performance::check_model(exp3_egg_glm2, check = c("qq"))
# qq looks better
#  homogenity looks ok 
# stick with this 

#  using the chosen model for data analysis 
# summary function to look at anova 
summary(exp3_egg_glm2)

# trying anova code 
anova(exp3_egg_glm2)

#-- analysing all egg data using tukey emmeans 
emmeans::emmeans(exp3_egg_glm2, specs = pairwise ~ diet)

