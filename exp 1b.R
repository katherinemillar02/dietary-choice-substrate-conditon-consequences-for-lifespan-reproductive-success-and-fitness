exp1ball 



exp1bfeeding_summary

exp1bd2feeding_summary

exp1b_all_day_lm_3 <- lm(formula = log(fly_numbers + 1) ~  diet * day , data = exp1ball)

performance::check_model(exp1b_all_day_lm_3)
performance::check_model(exp1b_all_day_lm_3, check = c("qq"))
performance::check_model(exp1b_all_day_lm_3, check = c("homogeneity"))
performance::check_model(exp1b_all_day_lm_3, check = c("linearity"))
performance::check_model(exp1b_all_day_lm_3, check = c("outliers"))

MASS::boxcox(exp1b_all_day_lm_3 )

emmeans::emmeans(exp1b_all_day_lm_3, pairwise ~ diet * day)

exp(1.397 )
exp(0.479)
exp(0.199)

exp1b_all_day_glm_3 <- glm(formula = sqrt(fly_numbers + 1) ~  diet * day, family = quasipoisson(link = "log"), data = exp1ball)

performance::check_model(exp1b_all_day_glm_3)
performance::check_model(exp1b_all_day_glm_3, check = c("qq"))
performance::check_model(exp1b_all_day_glm_3, check = c("homogeneity"))

drop1(exp1b_all_day_glm_3,test = "F")
summary(exp1b_all_day_glm_3)

emmeans::emmeans(exp1b_all_day_glm_3, pairwise ~ diet * day)

exp1ball$food_type <- ifelse(exp1ball$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
exp1ball$food_nutrition <- ifelse(exp1ball$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")

exp1b_combined_foodconditions_lm <- lm(fly_numbers ~ food_type * food_nutrition * day, data = exp1ball)


performance::check_model(exp1b_combined_foodconditions_lm )
performance::check_model(exp1b_combined_foodconditions_lm, check = c("qq"))


performance::check_model(exp1b_combined_foodconditions_lm, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_lm, check = c("linearity"))
performance::check_model(exp1b_combined_foodconditions_lm, check = c("outliers"))



exp1b_combined_foodconditions_glm <- glm(fly_numbers ~ food_type * food_nutrition * day, family=quasipoisson, data = exp1ball)

summary(exp1b_combined_foodconditions_glm )


performance::check_model(exp1b_combined_foodconditions_glm )
performance::check_model(exp1b_combined_foodconditions_glm, check = c("qq"))
performance::check_model(exp1b_combined_foodconditions_glm, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_glm, check = c("linearity"))
performance::check_model(exp1b_combined_foodconditions_glm, check = c("outliers"))



exp1b_combined_foodconditions_lm_2 <- lm(formula = log(fly_numbers + 1) ~ food_type * food_nutrition * day, data = exp1ball)


performance::check_model(exp1b_combined_foodconditions_lm_2 )
performance::check_model(exp1b_combined_foodconditions_lm_2, check = c("qq"))


performance::check_model(exp1b_combined_foodconditions_lm_2, check = c("homogeneity"))
performance::check_model(exp1b_combined_foodconditions_lm, check = c("linearity"))
performance::check_model(exp1b_combined_foodconditions_lm, check = c("outliers"))

MASS::boxcox(exp1b_combined_foodconditions_lm_2)


summary(exp1b_combined_foodconditions_lm_2)


exp1all_plot + exp1ball_plot 


boxplot_food_fc_e1b_fh_e1 <- ggplot()+ 
  geom_boxplot(exp1ball, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Experiment 1b")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1ball,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


boxplot_food_fc_e1b_fn_e1 <- ggplot()+ 
  geom_boxplot(exp1ball, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Experiment 1b")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1ball,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


boxplot_food_fc_e1a_fh_e1 + boxplot_food_fc_e1a_fn_e1 + boxplot_food_fc_e1b_fh_e1 + boxplot_food_fc_e1b_fn_e1



boxplot_food_fc_e1a_fh_e1 <- ggplot()+ 
  geom_boxplot(exp1a_all, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Experiment 1a")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1a_all,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


boxplot_food_fc_e1a_fn_e1 <- ggplot()+ 
  geom_boxplot(exp1a_all, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Experiment 1a")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1a_all,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
