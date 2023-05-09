# reading the calculated foxo data in 
foxo_calcs <- read_excel("data/foxo_calcs.xlsx")

# making a summary of the foxo data 
foxo_sum <- newlong_foxo_calcs%>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T),
            sd = sd(cq, na.rm = T),
            n = n(),
            se = sd/sqrt(n))


# linear model 
qpcr_foxo_lm <- lm(cq ~ sample, data = newlong_foxo_calcs)

# performance check 
performance::check_model(qpcr_foxo_lm)
performance::check_model(qpcr_foxo_lm, check = c("qq"))
performance::check_model(qpcr_foxo_lm, check = c("linearity"))
performance::check_model(qpcr_foxo_lm, check = c("homogeneity"))

# summary for analysis 
summary(qpcr_foxo_lm)

# generalised linear model 
qpcr_foxo_glm <- glm(cq ~ sample, family = poisson, data = newlong_foxo_calcs)
# looks like it is overdispersed
# trying with quasipoisson 
qpcr_foxo_glm_2 <- glm(cq ~ sample, family = quasipoisson, data = newlong_foxo_calcs)

# performance check 
performance::check_model(qpcr_foxo_glm_2)
performance::check_model(qpcr_foxo_glm_2, check = c("qq"))

# summary for analysis 
summary(qpcr_foxo_glm_2) 

# tukey test with emmeans 
emmeans::emmeans(qpcr_foxo_glm_2, pairwise ~ sample) 
emmeans::emmeans(qpcr_foxo_lm , pairwise ~ sample) 



