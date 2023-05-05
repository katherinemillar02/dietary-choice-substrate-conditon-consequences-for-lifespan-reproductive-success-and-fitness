# oviposition data analysis experiment 2 
# food hardness 
foodhardness_boxplot_exp2_oviposition <- ggplot()+ 
  geom_boxplot(long_egg_counting2, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Substrate condition of diet",
       y = "Eggs per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting2,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

# nutrient composition
foodnutrition_boxplot_exp2_oviposition  <- ggplot()+ 
  geom_boxplot(long_egg_counting2, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("#999933", "#44AA99"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Eggs per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting2,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# using patchwork to combine the plots  
foodhardness_boxplot_exp2_oviposition + foodnutrition_boxplot_exp2_oviposition 
