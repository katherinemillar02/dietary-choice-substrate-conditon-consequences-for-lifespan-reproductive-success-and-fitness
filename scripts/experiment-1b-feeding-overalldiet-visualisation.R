

# Using the summary data to create a bar plot of day 1 data 
exp1bfeeding_plotd1 <- exp1bfeeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge1bd1,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9.0)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "Day 1")+
  theme_classic() 



#- visualising the data for just day 2 
exp1bfeeding_plotd2 <- exp1bd2feeding_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_feedinge1bd2,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9.0)+
  labs(x = "Diet \n(Protein: Carbohydrate)",
       y = "Mean (+/- S.E.) number of flies on a patch",
       title = "Day 2")+
  theme_classic() 


# comparing the days using patchwork
exp1bfeeding_plotd1 + exp1bfeeding_plotd2 