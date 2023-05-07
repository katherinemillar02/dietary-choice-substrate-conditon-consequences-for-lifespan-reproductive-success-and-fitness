

# Reading the data in 
egg_counting_data_e2 <- (read_excel(path = "data/RPEggCountE2.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting2 <- egg_counting_data_e2 %>% 
  pivot_longer(cols = ("8:1S":"1:2H"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting2_summary <- long_egg_counting2 %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))
#-- Making a linear model 
exp2_egg_lm <- lm(egg_numbers ~ diet, data = long_egg_counting2)
#---- Checking the model 
performance::check_model(exp2_egg_lm)
performance::check_model(exp2_egg_lm, check = c("qq"))
performance::check_model(exp2_egg_lm, check = c("linearity"))
# model doesn't look very good

#-- Making a generalised linear model 
exp2_egg_glm <- glm(egg_numbers ~ diet, family = poisson, data = long_egg_counting2)

# using summary() to check for overdispersion
summary(exp2_egg_glm)

# using quasipoission as is overdispersed 
exp2_egg_glm2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting2)

#---- Checking the model 
performance::check_model(exp2_egg_glm2)
performance::check_model(exp2_egg_glm2, check = c("qq"))
# homogenity looks ok but still a alight slope
# go with this 

# doing data analysis for the chosen model 
summary(exp2_egg_glm2)

#-- emmeans tukey to look for significance of everything 
emmeans::emmeans(exp2_egg_glm2, specs = pairwise ~ diet)

#---- one-way anova? 
anova(exp2_egg_glm2) 

#-- looking at the confidence intervals 
confint(exp2_egg_glm2)

# tidyverse summary
broom::tidy(exp2_egg_glm2,  
            exponentiate=T, 
            conf.int=)

# visualising the means
GGally::ggcoef_model(exp2_egg_glm2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)
