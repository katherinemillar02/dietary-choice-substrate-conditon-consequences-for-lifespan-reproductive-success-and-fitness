

long_egg_counting3$food_type <- ifelse(long_egg_counting3 $diet %in% c("8:1H", "1:8H"), "Hard", "Soft")
long_egg_counting3$food_nutrition <- ifelse(long_egg_counting3 $diet %in% c("8:1", "1:8H", "1:8S"), "1:8", "8:1")




# comparing soft and hard foods 
foodhardness_boxplot_exp3_oviposition <- long_egg_counting3 %>% 
  ggplot(aes(x = food_type, y = egg_numbers, fill = food_type))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Substrate condition of diet",
       y = "Flies per food patch", 
       title = "Food Hardness")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting3,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# comparing 1:2 and 8:1 diets
nutrientcompositon_boxplot_exp3_oviposition <- long_egg_counting3 %>%
  ggplot(aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#CF9FFF", "#FF8C69"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Flies per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data =  long_egg_counting3,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# using patchwork to visualise the data 
foodhardness_boxplot_exp3_oviposition + nutrientcompositon_boxplot_exp3_oviposition 


# using barplots
# food hardness bar plot 
softhardegg_plot_exp3 <- softhardegg_summary_exp3 %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
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


# nutrient composition barplot 
nutrientegg_plot_exp3 <- nutrientegg_summary_exp3 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting3,
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
# combining the experiment hardness plot with the nutrient plot with patchwork 
softhardegg_plot_exp3 + nutrientegg_plot_exp3
