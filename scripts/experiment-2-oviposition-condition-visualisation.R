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
  scale_fill_manual(values=c("#FFAC1C", "#44AA99"))+
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


# creating a bar plot of food hardness egg count data 
softhardegg_plot <- softhardegg_summary %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting2,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs on a patch",
       title = "")+
  theme_classic() 



# creating a bar plot of nutrient composition egg count data
nutrientegg_plot <- nutrientegg_summary %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting2,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs on a patch",
       title = "")+
  theme_classic() 

# using patchwork to visualise the data of food hardness and nutrient composition together
softhardegg_plot + nutrientegg_plot
