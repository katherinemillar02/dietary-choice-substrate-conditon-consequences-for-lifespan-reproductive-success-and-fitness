# EGG COUNTING ANALYSIS 
# NEW SCRIPT WITH PROPERLY CHECKED MODELS 

#____ Reading the data in 
egg_counting_data_1b <- (read_excel(path = "data/RPEggCountE1b.xlsx", na = "NA"))
#____ Making the data long 
long_egg_counting1b <- egg_counting_data_1b %>% 
  pivot_longer(cols = ("1:2H":"1:8S"), names_to = "diet", values_to = "egg_numbers")
#_____ Making a summary of the data 
egg_counting1_summary_1b <- long_egg_counting1b %>% 
  group_by(diet) %>% 
  summarise(mean = mean(egg_numbers),
            sd = sd(egg_numbers),
            n = n(),
            se = sd/sqrt(n))


# Doing data analysis for overall egg numbers to diet

# Linear model 
eggcountinge1b_lm <- lm(egg_numbers ~ diet, data = long_egg_counting1b)

# Checking the model 
performance::check_model(eggcountinge1b_lm)
performance::check_model(eggcountinge1b_lm, check = c("qq"))
performance::check_model(eggcountinge1b_lm, check = c("homogeneity"))
performance::check_model(eggcountinge1b_lm, check = c("outliers"))
# qq and homogenity do not look great 

# Trying a glm
eggcountinge1b_glm <- glm(egg_numbers ~ diet, family = poisson, data = long_egg_counting1b)

# Performance checking the glm
performance::check_model(eggcountinge1b_glm)
performance::check_model(eggcountinge1b_glm, check = c("qq"))
performance::check_model(eggcountinge1b_glm, check = c("homogeneity"))
performance::check_model(eggcountinge1b_glm, check = c("outliers"))
# Looks a lot better 

# Using summary function to check for overdispersion 
summary(eggcountinge1b_glm)
# Overdispersed

# Model is overdispersed so using quasipoisson
eggcountinge1b_glm_2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting1b)

# Performance checking the glm
performance::check_model(eggcountinge1b_glm_2)
performance::check_model(eggcountinge1b_glm_2, check = c("qq"))
performance::check_model(eggcountinge1b_glm_2, check = c("homogeneity"))
performance::check_model(eggcountinge1b_glm_2, check = c("outliers"))
# Model looks a lot better 

# Getting results from summary 
summary(eggcountinge1b_glm_2)

# Tukey test to analyse everything 
emmeans::emmeans(eggcountinge1b_glm_2, pairwise ~ diet)








