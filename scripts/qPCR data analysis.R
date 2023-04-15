# USE FROM HERE ------------

# reading the summarised foxo data in 
foxo_calcs <- read_excel("data/foxo_calcs.xlsx")



# making the summarised foxo data long 
newlong_foxo_calcs <- foxo_calcs %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")

# making a summary of the foxo data 
foxo_sum <- newlong_foxo_calcs%>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T),
            sd = sd(cq, na.rm = T),
            n = n(),
            se = sd/sqrt(n))


foxo_plot <- foxo_sum %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#000080",
           alpha = 0.6)+
  theme_classic()+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#000080",
                width = 0.2)+
  labs(title = "foxo",
       x = "Diet larvae were reared on \n(Protein: Carbohydrate/ Food Hardness)",
       y = "Mean average relative expression 2^-ΔCt +/- S.E. fd38 + rp20")+
  geom_jitter(data = newlong_foxo_calcs,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



# DATA ANALYSIS -----

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
