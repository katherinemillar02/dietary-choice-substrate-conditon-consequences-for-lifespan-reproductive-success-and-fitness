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




ggplot(exp2both, aes(x = diet, y = fly_numbers)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Four Diets with Varying Mean Averages") + 
  xlab("Diet Type") + 
  ylab("Average")+
  COLOUR("orange")


boxplot(fly_numbers~diet,
        data=exp2both,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown"
)



p<-ggplot(exp2both, aes(x=diet, y=fly_numbers, color=diet)) +
  geom_boxplot()+
  theme_classic()
p

p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))
p+scale_color_brewer(palette="Dark2")
p + scale_color_grey() + theme_classic()


ggplot(exp2both, aes(x=diet, y=fly_numbers)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()


p<-ggplot(exp2both, aes(x=diet, y=fly_numbers, fill=diet)) +
  geom_boxplot()+
  theme_classic()
p




p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))


p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend


p + scale_fill_brewer(palette="Dark2") + theme_minimal()



p<-ggplot(exp2both , aes(x=food_type, y=fly_numbers, fill=food_type)) +
  geom_boxplot()+
  theme_classic()
p

a<-ggplot(exp2both, aes(x=food_nutrition, y=fly_numbers, fill=food_nutrition)) +
  geom_boxplot()+
  theme_classic()

p + exp2both_plot

exp2both_plot + egg_counting2_plot + p  + a + nutrientegg_plot + softhardegg_plot + p

 