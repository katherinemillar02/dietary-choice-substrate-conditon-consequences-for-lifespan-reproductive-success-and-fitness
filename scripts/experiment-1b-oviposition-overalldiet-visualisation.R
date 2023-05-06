
# DATA VISUALISATION FOR EGG COUNTING OVERALL DIETS EXPERIMENT 1B

#---------- Visualise the data of egg counting
egg_counting1b_plot <- egg_counting1_summary_1b %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "orange",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "orange",
                width = 0.2)+
  geom_jitter(data = long_egg_counting1b,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "black",
              width = 0.2,
              shape = 21)+
  ylim(0,200)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of eggs laid on each patch",
       title = "Overall Diets")+
  theme_classic()

