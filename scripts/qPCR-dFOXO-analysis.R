# visualising the foxo data
foxo_plot <- foxo_sum %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#000080",
           alpha = 0.6)+
  theme_classic()+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#000080",
                width = 0.2)+
  labs(title = "foxo",
       x = "Diet larvae were reared on \n(Protein: Carbohydrate/ Food Hardness)",
       y = "Mean average relative expression 2^-ΔCt +/- S.E. fd38 + rp20")+
  geom_jitter(data = newlong_foxo_calcs,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


