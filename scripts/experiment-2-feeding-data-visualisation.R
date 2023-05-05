# NEW SCRIPT TO LOOK AT DATA VISUALISATION 
# EDITING COLOURS OF BOXPLOTS 


# Splitting data into new variables to look at food hardness and nutrient composition seperatley
exp2_combined$food_type <- ifelse(exp2_combined$diet %in% c("8:1H", "1:2H"), "Hard", "Soft")
exp2_combined$food_nutrition <- ifelse(exp2_combined$diet %in% c("8:1", "1:2H", "1:2S"), "1:2", "8:1")




# comparing soft and hard foods 
foodhardness_boxplot_exp2_feeding <- exp2_combined  %>% 
  ggplot(aes(x = food_type, y = fly_numbers, fill = food_type))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Substrate condition of diet",
       y = "Flies per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp2_combined,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# comparing 1:2 and 8:1 diets
nutrientcompositon_boxplot_exp1b_feeding <- exp2_combined %>%
  ggplot(aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#CF9FFF", "#FF8C69"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Flies per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp2_combined,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# Using patchwork to combine the plots together 

foodhardness_boxplot_exp2_feeding + nutrientcompositon_boxplot_exp1b_feeding

