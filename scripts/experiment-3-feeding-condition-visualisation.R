# DOING DATA VISUALISATION FOR EXPERIMENT 3 



long_feedinge3d2$food_type <- ifelse(long_feedinge3d2$diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
long_feedinge3d2$food_nutrition <- ifelse(long_feedinge3d2$diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")





# comparing soft and hard foods 
foodhardness_boxplot_exp3_feeding_d2 <- long_feedinge3d2  %>% 
  ggplot(aes(x = food_type, y = fly_numbers, fill = food_type))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Substrate condition of diet",
       y = "Flies per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  long_feedinge3d2,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

# visualising data for 1:8 and 8:1 
nutrientcompositon_boxplot_exp3_feeding_d2 <- long_feedinge3d2 %>%
  ggplot(aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#CF9FFF", "#FF8C69"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Flies per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data =  long_feedinge3d2,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# using patchwork to compare the plots 
foodhardness_boxplot_exp3_feeding_d2 + nutrientcompositon_boxplot_exp3_feeding_d2


