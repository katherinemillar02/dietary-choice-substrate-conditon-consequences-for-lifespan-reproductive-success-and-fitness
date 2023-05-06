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


# comparing 1:2 and 8:1 foods 
nutrientcompositon_boxplot_exp2_feeding <- exp2_combined %>%
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

foodhardness_boxplot_exp2_feeding + nutrientcompositon_boxplot_exp2_feeding




# creating barplots to look at the food condition data 

# a bar plot of soft vs hard diets in experiment 2 (days combined)
softhard_plot_exp2 <- softhard_summary_exp2 %>% 
  ggplot(aes(x = food_type, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp2_combined,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 


# a barplot of 1:2 vs 8:1 diets in experiment 2 (days combined)
nutrient_plot_exp2 <- nutrient_summary_exp2 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = exp2_combined,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9)+
  labs(x = "Diet \n(Protein; Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "")+
  theme_classic() 



# using patchwork to compare soft/hardness and nutrient composition
softhard_plot_exp2 + nutrient_plot_exp2

