foxoqPCR <- read_excel("data/qPCR_foxo_data.xlsx")
long_foxoqPCR <- foxoqPCR %>% 
  pivot_longer(cols = ("A1":"D3"), names_to = "sample", values_to = "cq")

is.na(foxoqPCR)

newlong_foxoqPCR <- newfoxoqpcr %>% 
  pivot_longer(cols = ("A1":"D3"), names_to = "sample", values_to = "cq")


newfoxoqpcr <- na.omit(foxoqPCR)

foxoqPCR_summary <- long_foxoqPCR %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))

foxoqPCR_plotd1 <- foxoqPCR_summary %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_foxoqPCR,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 50)+
  labs(x = "Diet Larvae were grown on",
       y = "Mean Cq",
       title = "")+
  theme_minimal() 


is.na(foxoqPCR2)


foxoqPCR2 <- read_excel("data/qPCR_foxo_data_2.xlsx", na = "NA")
long_foxoqPCR2 <- foxoqPCR2 %>% 
  pivot_longer(cols = ("A":"D"), names_to = "sample", values_to = "cq")

foxoqPCR2_summary <- long_foxoqPCR2 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))

foxoqPCR_plot2 <- foxoqPCR2_summary %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = long_foxoqPCR2,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 50)+
  labs(x = "Diet larvae grew on \n(Protein: Carbohydrate)",
       y = "Mean Cq", 
       title = "Foxo")+
  theme_classic()

#%>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 



install.packages("pcr")
library(pcr)

 
