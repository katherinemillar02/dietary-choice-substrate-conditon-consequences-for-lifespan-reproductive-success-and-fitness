# Load the ggplot2 library
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Create the boxplot
ggplot(egg_counting1_summary, aes(x = diet, y = mean)) +
  geom_boxplot() +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(title = "Boxplot of Egg Counts by Diet",
       x = "Diet", y = "Mean Egg Count")+
  theme_classic()




# Create the boxplot


egg_counting1_boxplot <- egg_counting1_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_boxplot(stat = "identity",
               fill = "skyblue",
               colour = "orange",
               alpha = 0.6)+
  theme_classic()


exp2both%>%   group_by(Colour)%>%   dplyr::summarise(Mean=mean(food_type)) #calculatng means 
Data$COLOUR<-as.factor(Data$Colour) #puts data into correct format Data$SECONDS<-as.numeric(Data$Seconds) 

library(ggplot2) 

boxplot<-ggplot()+ geom_boxplot(exp2both, mapping=aes(x=food_nutrition, y=mean,fill=food_nutrition))+ 
  theme_classic()+
  scale_fill_manual(values=c("blue","gold","pink"))





nutrient_boxplot_exp2 <- nutrient_summary_exp2 %>% 
  ggplot(aes(x = food_nutrition, y = mean))+
  geom_boxplot(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)

