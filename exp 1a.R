exp1a_all_day_lm_2 <- lm(formula = log(fly_numbers + 1) ~ day * diet , data = exp1a_all)

summary(exp1a_all_day_lm_2)

drop1(exp1a_all_day_lm_2, test = "F")

exp1a_all_day_lm_3 <- lm(formula = log(fly_numbers + 1) ~  diet , data = exp1a_all)

summary(exp1a_all_day_lm_3)

emmeans::emmeans(exp1a_all_day_lm_3, pairwise ~ diet)


exp1a_all


exp1a_all$food_type <- ifelse(exp1a_all$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
exp1a_all$food_nutrition <- ifelse(exp1a_all$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")

exp1_combined_foodconditions_lm <- lm(fly_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = exp1a_all)

# checking the new  linear model 
performance::check_model(exp1_combined_foodconditions_lm)
performance::check_model(exp1_combined_foodconditions_lm, check = c("qq"))
performance::check_model(exp1_combined_foodconditions_lm, check = c("homogeneity"))


exp1_combined_foodconditions_lm_2 <- lm(formula = log(fly_numbers + 1) ~ food_type * food_nutrition * day, data = exp1a_all)

performance::check_model(exp1_combined_foodconditions_lm_2)
performance::check_model(exp1_combined_foodconditions_lm_2, check = c("qq"))
performance::check_model(exp1_combined_foodconditions_lm_2, check = c("homogeneity"))

MASS::boxcox(exp1_combined_foodconditions_lm_2 )

summary(exp1_combined_foodconditions_lm_2)


exp1_combined_foodconditions_glm <- glm((fly_numbers +1) ~ food_type * food_nutrition , family = quasipoisson(link = "log"), data = exp1a_all)

performance::check_model(exp1_combined_foodconditions_glm)
performance::check_model(exp1_combined_foodconditions_glm, check = c("qq"))
performance::check_model(exp1_combined_foodconditions_glm, check = c("outliers"))
performance::check_model(exp1_combined_foodconditions_glm, check = c("homogeneity"))


drop1(exp1_combined_foodconditions_glm, test = "F")

summary(exp1_combined_foodconditions_glm)







eggcountinge1_lm <- lm(formula = log(egg_numbers + 1) ~ diet, data = long_egg_counting1)

performance::check_model(eggcountinge1_lm, check = c("qq"))



eggcountinge1_glm <- glm(formula = log(egg_numbers + 1) ~ diet, family = quasipoisson, data = long_egg_counting1)

summary(eggcountinge1_glm)

emmeans::emmeans(eggcountinge1_glm, pairwise ~ diet)

performance::check_model(eggcountinge1_glm)
performance::check_model(eggcountinge1_glm, check = c("qq"))
performance::check_model(eggcountinge1_glm,  check = c("outliers"))
performance::check_model(eggcountinge1_glm,  check = c("homogeneity"))


MASS::boxcox(eggcountinge1_glm)

MASS::boxcox(eggcountinge1_glm)

long_egg_counting1$food_type <- ifelse(long_egg_counting1$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
long_egg_counting1$food_nutrition <- ifelse(long_egg_counting1$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")



exp1_egg_combined_foodconditions_lm <- lm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, data = long_egg_counting1)

performance::check_model(exp1_egg_combined_foodconditions_lm)
performance::check_model(exp1_egg_combined_foodconditions_lm, check = c("qq"))
performance::check_model(eggcountinge1_glm,  check = c("outliers"))
performance::check_model(eggcountinge1_glm,  check = c("homogeneity"))


exp1_egg_combined_foodconditions_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = long_egg_counting1)

summary(exp1_egg_combined_foodconditions_glm)

performance::check_model(exp1_egg_combined_foodconditions_glm)
performance::check_model(exp1_egg_combined_foodconditions_glm, check = c("qq"))
performance::check_model(exp1_egg_combined_foodconditions_glm, check = c("homogeneity"))

exp1_egg_combined_foodconditions_glm_2 <- glm(formula = log(egg_numbers + 1) ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = long_egg_counting1)

exp1_egg_combined_foodconditions_glm_3 <- glm(formula = log(egg_numbers + 1) ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = long_egg_counting1)


performance::check_model(exp1_egg_combined_foodconditions_glm_2)
performance::check_model(exp1_egg_combined_foodconditions_glm_2, check = c("qq"))
performance::check_model(exp1_egg_combined_foodconditions_glm_2, check = c("homogeneity"))
performance::check_model(exp1_egg_combined_foodconditions_glm_2, check = c("outliers"))

exp(2.67) -1
exp(1.43) -1

summary(exp1_egg_combined_foodconditions_glm_2)

emmeans::emmeans(exp1_egg_combined_foodconditions_glm_2, pairwise ~ food_nutrition * food_type)
exp1_egg_combined_foodconditions_lm_prac <- lm(egg_numbers  ~ food_type * food_nutrition, data = long_egg_counting1)

emmeans::emmeans(exp1_egg_combined_foodconditions_lm_prac, pairwise ~ food_nutrition * food_type)

performance::check_model(exp1_egg_combined_foodconditions_glm_2, check = c("homogeneity"))


long_egg_counting1b$food_type <- ifelse(long_egg_counting1b$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
long_egg_counting1b$food_nutrition <- ifelse(long_egg_counting1b$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")

egg_counting1_plot + egg_counting1b_plot


eggcountinge1b_lm_5 <- lm(egg_numbers ~ diet, data = long_egg_counting1b)
summary(eggcountinge1b_lm_5)

performance::check_model(eggcountinge1b_lm_5)
performance::check_model(eggcountinge1b_lm_5, check = c("qq"))
performance::check_model(eggcountinge1b_lm_5, check = c("homogeneity"))
performance::check_model(eggcountinge1b_lm_5, check = c("outliers"))


eggcountinge1b_lm_6 <- lm(formula = log(egg_numbers + 1) ~ diet, data = long_egg_counting1b)

summary(eggcountinge1b_lm_6)
performance::check_model(eggcountinge1b_lm_6, check = c("homogeneity"))

emmeans::emmeans(eggcountinge1b_lm_6, pairwise ~ diet)

performance::check_model(eggcountinge1b_lm_6, check = c("qq"))


eggcountinge1b_glm_2 <- glm(egg_numbers ~ diet, family = quasipoisson, data = long_egg_counting1b)

summary(eggcountinge1b_glm_2)
emmeans::emmeans(eggcountinge1b_glm_2, pairwise ~ diet)

emmeans::emmeans(eggcountinge1b_glm, pairwise ~ diet)
summary(eggcountinge1b_glm)

confint(eggcountinge1b_glm)

confint.lm(eggcountinge1b_lm)

performance::check_model(eggcountinge1b_glm)
performance::check_model(eggcountinge1b_glm , check = c("qq"))
performance::check_model(eggcountinge1b_glm , check = c("homogeneity"))

exp1b_egg_combined_foodconditions_glm <- glm(egg_numbers ~ food_type + food_nutrition + food_type : food_nutrition, family = quasipoisson, data = long_egg_counting1b)

performance::check_model(exp1b_egg_combined_foodconditions_glm)
performance::check_model(exp1b_egg_combined_foodconditions_glm, check = c("qq"))
performance::check_model(exp1b_egg_combined_foodconditions_glm, check = c("homogeneity"))

summary(exp1b_egg_combined_foodconditions_glm)
emmeans::emmeans(exp1b_egg_combined_foodconditions_glm, pairwise ~ food_type + food_nutrition )



eggcountinge1b_lm_6_foodcondition <- lm(formula = log(egg_numbers + 1) ~ food_nutrition + food_type + food_nutrition:food_type, data = long_egg_counting1b)

eggcountinge1b_lm_7_foodcondition <- lm(egg_numbers  ~ food_nutrition + food_type + food_nutrition:food_type, data = long_egg_counting1b)


performance::check_model(eggcountinge1b_lm_7_foodcondition, check = c("qq"))
performance::check_model(eggcountinge1b_lm_7_foodcondition, check = c("homogeneity"))

performance::check_model(eggcountinge1b_lm_6_foodcondition)
performance::check_model(eggcountinge1b_lm_6_foodcondition, check = c("qq"))
performance::check_model(eggcountinge1b_lm_6_foodcondition, check = c("homogeneity"))

summary(eggcountinge1b_lm_6_foodcondition)

boxplot_food_fc_e1b_fh_e1_egg <- ggplot()+ 
  geom_boxplot(long_egg_counting1b, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Experiment 1b")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting1b,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

boxplot_food_fc_e1b_fh_e1_egg + boxplot_food_fc_e1b_fh_e1a_egg

boxplot_food_fc_e1b_fh_e1a_egg <- ggplot()+ 
  geom_boxplot(long_egg_counting1, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Experiment 1a")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting1,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



boxplot_food_fc_e1b_fn_e1a_egg <- ggplot()+ 
  geom_boxplot(long_egg_counting1, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Nutrition",
       y = "Mean average flies per patch", 
       title = "Experiment 1a")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting1,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


hey  <- egg_counting1_plot + egg_counting1b_plot 

 boxplot_food_fc_e1b_fh_e1a_egg + boxplot_food_fc_e1b_fn_e1a_egg + boxplot_food_fc_e1b_fh_e1_egg + boxplot_food_fc_e1b_fn_e1_egg  

 
 
 boxplot_food_fc_e1b_fh_e1a_egg + boxplot_food_fc_e1b_fh_e1_egg
 
 
 
boxplot_food_fc_e1b_fn_e1_egg <- ggplot()+ 
  geom_boxplot(long_egg_counting1b, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Nutrition",
       y = "Mean average flies per patch", 
       title = "Experiment 1b")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting1b,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)






boxplot_foodcondition_e1a_d2 <- ggplot()+ 
  geom_boxplot(long_feedinge1d2, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Nutrition",
       y = "Mean average flies per patch", 
       title = "Experiment 1b")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_feedinge1d2,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




