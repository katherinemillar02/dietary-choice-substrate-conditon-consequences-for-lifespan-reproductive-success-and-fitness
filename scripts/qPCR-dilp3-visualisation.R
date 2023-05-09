# visualising the data with a dilp3 plot 
dilp3_plot <- dilp3_sum %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "darkgreen",
           alpha = 0.6)+
  theme_classic()+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "darkgreen",
                width = 0.2)+
  labs(title = "dilp3",
       x = "Diet larvae were reared on \n(Protein: Carbohydrate/ Food Hardness)",
       y = "Mean average relative expression 2^-ΔCt +/- S.E. fd38 + rp20")+
  geom_jitter(data = newlong_dilp3_calcs,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
