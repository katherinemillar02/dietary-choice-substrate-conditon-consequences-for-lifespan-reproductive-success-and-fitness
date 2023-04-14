# Load the ggplot2 library
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)




# experiment 1 
# overall food bar plot 
exp1_combined_plot

# food nutrition and food hardness bar plot 
softhard_plot_exp1 + nutrient_plot_exp1


# food nutrition and food hardness box plot 
exp1_fh_boxplot <- ggplot()+ 
  geom_boxplot(exp1_combined, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch")+
  theme(legend.position="none")+
  ylim(0,9)

exp1_fn_boxplot <- ggplot()+ 
  geom_boxplot(exp1_combined, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Nutrient Composition",
       y = "Mean average flies per patch")+
  theme(legend.position="none")+
  ylim(0,9)

exp1_fh_boxplot + exp1_fn_boxplot 


# experiment 1 barplots together 
exp1_combined_plot + softhard_plot_exp1 + nutrient_plot_exp1

# experiment 1 boxplots together 
exp1_combined_plot + exp1_fh_boxplot + exp1_fn_boxplot

boxplot_egg_fc_e1_egg <- ggplot()+ 
  geom_boxplot(exp1_egg_combined, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Hardness",
       y = "Mean average eggs per patch")+
  theme(legend.position="none")+
  ylim(0,200)

boxplot_food_fc_e1_fn_egg <- ggplot()+ 
  geom_boxplot(exp1_egg_combined, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Nutrient Composition",
       y = "Mean average eggs per patch")+
  theme(legend.position="none")+
  ylim(0,200)


# experiment 1 egg bar plots together 
exp1_egg_combined_plot + softhard_plot_exp1_egg + nutrient_plot_exp1_egg

exp1_egg_combined_plot + boxplot_egg_fc_e1_egg + boxplot_food_fc_e1_fn_egg 




boxplot1 <- ggplot()+ 
  geom_boxplot(exp2both, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c( "gold", "pink"))+
  theme(legend.position="none")+
  ylim(0,9)+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Flies per food patch",
       title = "Nutrient Composition")+
  geom_jitter(data = exp2_combined,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
  


boxplot2 <- ggplot()+ 
  geom_boxplot(exp2_combined, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+ 
  theme_classic()+
  scale_fill_manual(values=c( "gold", "pink"))+
  theme(legend.position="none")+
  ylim(0,9)+
  labs(x = "Substrate condition of diet",
       y = "Flies per food patch", 
       title = "Food Hardness")+
  geom_jitter(data = exp2_combined,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




 boxplot2 + boxplot1

exp2_combined_plot +  boxplot2 + boxplot1

  
  
  softhard_plot_exp2 + nutrient_plot_exp2




boxplot2 <- ggplot()+ 
  geom_boxplot(exp2both, mapping=aes(x=diet, y=fly_numbers, fill=diet))+
  theme_classic()+
  scale_fill_manual(values=c("blue", "gold", "pink", "green"))


boxplot_egg_fc <- ggplot()+ 
  geom_boxplot(exp2both, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Nutrient Composition",
       y = "Mean average flies per patch")+
  theme(legend.position="none")+
  ylim(0,200)+ 
  geom_jitter(data = exp2_combined,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

boxplot_egg_fc_2 <- ggplot()+ 
  geom_boxplot(exp2both, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data = exp2_combined,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

boxplot_egg_fc + boxplot_egg_fc_2


boxplot_food_fc_e1 <- ggplot()+ 
  geom_boxplot(exp1_combined, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch")+
  theme(legend.position="none")+
  ylim(0,9)



exp1_combined_plot + boxplot_food_fc_e1 + boxplot_food_fc_e1_fn
exp1_combined_plot + softhard_plot_exp1 + nutrient_plot_exp1



exp1_egg_combined_plot + boxplot_egg_fc_e1 + boxplot_food_fc_e1_fn 




exp1_combined_plot + boxplot_food_fc_e1 + boxplot_food_fc_e1_fn

exp1_combined_plot + softhard_plot_exp1 + nutrient_plot_exp1

softhard_plot_exp1_egg + nutrient_plot_exp1_egg

boxplot_food_fc_e2 <- ggplot()+ 
  geom_boxplot(exp2_combined, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch")+
  theme(legend.position="none")

boxplot_food_fc_e2_fn <- ggplot()+ 
  geom_boxplot(exp2_combined, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Nutrient Composition",
       y = "Mean average flies per patch")+
  theme(legend.position="none")

exp2_combined_plot + boxplot_food_fc_e2 + boxplot_food_fc_e2_fn

# trying to read boxplots vs barplots 

boxplot_food_fc_e1 + boxplot_food_fc_e1_fn 
softhard_plot_exp1 + nutrient_plot_exp1

# same data but why do they look so different 



exp2_egg_foodtype <- ggplot()+ 
  geom_boxplot(long_egg_counting2, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Substrate condition of diet",
       y = "Mean average eggs per patch",
       title = "Food Hardness")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data = long_egg_counting2,
              aes(x = food_type,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
  

exp2_egg_foodnutrition <- ggplot()+ 
  geom_boxplot(long_egg_counting2, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Protein: Carbohydrate content of diet",
       y = "Eggs per food patch", 
       title = "Nutrient Composition")+
  theme(legend.position="none")+ 
  ylim(0,200)+
  geom_jitter(data = long_egg_counting2,
              aes(x = food_nutrition,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


egg_counting2_plot + exp2_egg_foodtype + exp2_egg_foodnutrition


egg_counting2_plot + softhardegg_plot + nutrientegg_plot


exp3feeding_plot_both + softhard_plot_exp3 + nutrient_plot_exp3

  




boxplot_egg_fc_e3 <- ggplot()+ 
  geom_boxplot(long_egg_counting3, mapping=aes(x=food_type, y=egg_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Food Hardness",
       y = "Mean average eggs per patch")+
  theme(legend.position="none")+
  ylim(0,200)

boxplot_food_fc_e3_fn <- ggplot()+ 
  geom_boxplot(long_egg_counting3, mapping=aes(x=food_nutrition, y=egg_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("lightgreen", "lightblue"))+
  labs(x = "Nutrient Composition",
       y = "Mean average eggs per patch")+
  theme(legend.position="none")+ 
  ylim(0,200)


egg_counting2_plot + softhardegg_plot + nutrientegg_plot

egg_counting2_plot + boxplot_egg_fc_e2 + boxplot_food_fc_e2_fn 

exp3feeding_plot_both + boxplot_food_fc_e3 + boxplot_food_fc_e3_fn
  



boxplot_food_fc_e3 <- ggplot()+ 
  geom_boxplot(exp3_combined, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch")+
  theme(legend.position="none")+ 
  ylim(0,9)


boxplot_food_fc_e3_fn_d2 <- ggplot()+ 
  geom_boxplot(long_feedinge3d2, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Nutrient Composition",
       y = "Mean average flies per patch", 
       title = "Day 2")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data = long_feedinge3d2,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

boxplot_food_fc_e3_fn_d1 <- ggplot()+ 
  geom_boxplot(long_feedinge3d1, mapping=aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Nutrient Composition",
       y = "Mean average flies per patch", 
       title = "Day 1")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data = long_feedinge3d1,
              aes(x = food_nutrition,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)




boxplot_food_fc_e3_fh_d2 <- ggplot()+ 
  geom_boxplot(long_feedinge3d2, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Day 2")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data = long_feedinge3d2,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

boxplot_food_fc_e3_fh_d1 <- ggplot()+ 
  geom_boxplot(long_feedinge3d1, mapping=aes(x=food_type, y=fly_numbers, fill=food_type))+
  theme_classic()+
  scale_fill_manual(values=c("gold", "pink"))+
  labs(x = "Food Hardness",
       y = "Mean average flies per patch", 
       title = "Day 1")+
  theme(legend.position="none")+ 
  ylim(0,9)+
  geom_jitter(data = long_feedinge3d1,
              aes(x = food_type,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

  


boxplot_food_fc_e3_fh_d1 + boxplot_food_fc_e3_fn_d1 + boxplot_food_fc_e3_fh_d2  + boxplot_food_fc_e3_fn_d2
  
exp3feeding_plotd1  + exp3feeding_plotd2



 egg_counting3_plot + softhardegg_plot_exp3 + nutrientegg_plot_exp3

egg_counting3_plot + boxplot_egg_fc_e3 + boxplot_food_fc_e3_fn 

