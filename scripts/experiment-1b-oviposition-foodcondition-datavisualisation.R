# Food condition 
# Two factor analysis 

# Separating the factors of food hardness and nutrient composition
long_egg_counting1b$food_type <- ifelse(long_egg_counting1b$diet %in% c("1:8H", "1:2H"), "Hard", "Soft")
long_egg_counting1b$food_nutrition <- ifelse(long_egg_counting1b$diet %in% c("1:8", "1:2H", "1:2S"), "1:2", "1:8")






# Visualising the data fir oviposition choice 


# Food condition analysis 

# visualising the data

# food hardness boxplot for oviposition - experiment 1b
foodhardness_boxplot_exp1b_oviposition <- ggplot()+ 
  geom_boxplot(long_egg_counting1b, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Substrate condition of diet",
       y = "Eggs per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting1b,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# nutrient composition boxplot for oviposition - experiment 1b
foodnutrition_boxplot_exp1b_oviposition  <- ggplot()+ 
  geom_boxplot(long_egg_counting1b, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("#FFAC1C", "#44AA99"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Eggs per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting1b,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# Using patchwork to visualise the data together 
foodhardness_boxplot_exp1b_oviposition + foodnutrition_boxplot_exp1b_oviposition
