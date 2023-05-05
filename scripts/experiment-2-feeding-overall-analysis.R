# DATA ANALYSIS 
# EXPERIMENT 2 

#------------ Combined days -- data analysis  -----

# testing for the significance in day
exp2_combined_days_lm <- lm(fly_numbers ~ day, data = exp2_combined)

# checking the model
performance::check_model(exp2_combined_days_lm)
performance::check_model(exp2_combined_days_lm, check = c("qq"))
performance::check_model(exp2_combined_days_lm, check = c("linearity"))
# data doesn't look too great


# trying a glm 
exp2_combined_days_glm <- glm(fly_numbers ~ day, family = poisson, data = exp2_combined)

# using summary function to look for overdispersion
summary(exp2_combined_days_glm)
# overdispersed 

# trying a glm  with quasipoisson
exp2_combined_days_glm_2 <- glm(fly_numbers ~ day * diet, family = quasipoisson, data = exp2_combined)


# checking the model
performance::check_model(exp2_combined_days_glm_2)
performance::check_model(exp2_combined_days_glm_2, check = c("qq"))
# still doesn't look great but homogenity looks better 

# trying a glm + 1 
exp2_combined_days_glm_3 <- glm(formula = (fly_numbers + 1) ~ day * diet, family = quasipoisson, data = exp2_combined)

# checking the model
performance::check_model(exp2_combined_days_glm_3)
performance::check_model(exp2_combined_days_glm_3, check = c("qq"))
performance::check_model(exp2_combined_days_glm_3, check = c("outliers"))
performance::check_model(exp2_combined_days_glm_3, check = c("homogeneity"))
#  still looks bad at the beginning 
# ways to fix this? 

# from this the best model is exp2_combined_days_glm_3


# looking for significance in day
drop1(exp2_combined_days_glm_3, test = "F")

# summary shows completley different results? 
summary(exp2_combined_days_glm_3)
# day is significant but have dropped from the model 
# not significant in summary 


# DOING A MODEL WITHOUT DAY INCLUDED 

# analysing fly numbers and diet data 
# trying a linear model
exp2_combined_lm <- lm(fly_numbers ~ diet, data = exp2_combined)

# checking the model
performance::check_model(exp2_combined_lm)
performance::check_model(exp2_combined_lm, check = c("qq"))
performance::check_model(exp2_combined_lm, check = c("linearity"))
# qq looks sort of okay but linearity very uneven

# adding +1 to fly numbers as some are 0? 
exp2_combined_lm2 <- lm(formula = (fly_numbers + 1) ~ diet, data = exp2_combined)


# checking the model 2 
performance::check_model(exp2_combined_lm2)
performance::check_model(exp2_combined_lm2, check = c("qq"))
performance::check_model(exp2_combined_lm2, check = c("linearity"))
# the + 1 doesn't make a difference? 
# qq looks a bit bad

# trying a glm 
exp2_combined_glm <- glm(fly_numbers ~ diet, family = poisson, data = exp2_combined)

# using summary to look for overdispersion in the model
summary(exp2_combined_glm)

# model is overdispersed so doing quasipoisson
exp2_combined_glm2 <- glm(fly_numbers ~ diet, family = quasipoisson, data = exp2_combined)

# from this exp2_combined_lm2 is the best model but linearity dodgy on both

# data analysis with the chosen model 
summary(exp2_combined_lm2)

#- using emmeans to test the linear model in experiment 2 (without day in the model)
emmeans::emmeans(exp2_combined_lm2, specs = pairwise ~ diet)


# tidyverse version of a summary of data 
broom::tidy(exp2_combined_lm2, conf.int = T)


# Using GGally to look at the dispersion of means 
GGally::ggcoef_model(exp2_combined_lm2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

# looking for interaction effects between diets 
exp2_combined %>% ggplot(aes(x=fly_numbers, y=diet, colour = diet, fill = diet, group = diet))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )









