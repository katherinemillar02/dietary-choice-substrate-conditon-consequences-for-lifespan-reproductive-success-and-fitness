# 

# Visualizing the data 

# using boxplots to look at the different variables

# comparing soft and hard foods 
foodhardness_boxplot_exp1b_feeding <- exp1ball %>% 
  ggplot(aes(x = food_type, y = fly_numbers, fill = food_type))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Substrate condition of diet",
       y = "Flies per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1ball,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# comparing 1:2 and 1:8 diets
nutrientcompositon_boxplot_exp1b_feeding <- exp1ball %>%
  ggplot(aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#CF9FFF", "#FF8C69"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Flies per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  exp1ball,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

# using patchwork to compare the plots together 
foodhardness_boxplot_exp1b_feeding + nutrientcompositon_boxplot_exp1b_feeding
