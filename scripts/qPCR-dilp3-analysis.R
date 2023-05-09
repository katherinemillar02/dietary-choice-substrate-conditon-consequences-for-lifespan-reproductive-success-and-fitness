# reading the summarised dilp3 data in 
dilp3_calcs <- read_excel("data/dilp3_calcs.xlsx")

# making the  data long 
newlong_dilp3_calcs <- dilp3_calcs %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")
# making the data long summary
dilp3_sum <- newlong_dilp3_calcs %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T),
            sd = sd(cq, na.rm = T),
            n = n(),
            se = sd/sqrt(n))


# creating a linear model 
qpcr_dilp3_lm <- lm(cq ~ sample, data = newlong_dilp3_calcs)


# performance check 
performance::check_model(qpcr_dilp3_lm )
performance::check_model(qpcr_dilp3_lm , check = c("qq"))

# checking results of the linear model using summary 
summary(qpcr_dilp3_lm)

# checking results of the linear model using tukey emmeans
emmeans::emmeans(qpcr_dilp3_lm , pairwise ~ sample) 


# creating a generalised linear model with quasipoisson to count for overdispersion
qpcr_dilp3_glm <- glm(cq ~ sample, family = quasipoisson, data = newlong_dilp3_calcs)

# performance check 
performance::check_model(qpcr_dilp3_glm )
performance::check_model(qpcr_dilp3_glm , check = c("qq"))



# checking results of the generalised linear model
summary(qpcr_dilp3_glm)


# checking results of the generalised linear model using tukey emmeans
emmeans::emmeans(qpcr_dilp3_glm , pairwise ~ sample) 

