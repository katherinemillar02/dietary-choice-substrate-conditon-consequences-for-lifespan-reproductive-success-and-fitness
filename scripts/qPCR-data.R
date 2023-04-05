

# reading the foxo data in
foxoqPCR <- read_excel("data/qPCR_foxo_data_2.xlsx")
# removing the na values from the foxo data and changing the file name 
newfoxoqpcr <- na.omit(foxoqPCR)
view(newfoxoqpcr)
# making the new foxo data long 
newlong_foxoqPCR <- newfoxoqpcr %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
view(newlong_foxoqPCR)
#  making a summary of the foxo data 
newfoxoqPCR_summary <- newlong_foxoqPCR %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))
#  visualising the foxo data 
newfoxoqPCR_plotd1 <- newfoxoqPCR_summary %>% 
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



#  reading the fd38 data in 
fd38qPCR <- read_excel("data/fd38_qPCR.xlsx")
# removing the na values from the fd38 data 
newfd38qpcr <- na.omit(fd38qPCR)
view(newfd38qpcr)
# making the fd38 data long
newlong_fd38qPCR <- newfd38qpcr %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
view(newlong_fd38qPCR)
# making a summary of the fd38 data
newfd38qPCR_summary <- newlong_fd38qPCR %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))



#  reading the dilp3 data in
dilp3qPCR <- read_excel("data/qPCR_dilp3.xlsx")
# removing na values from dilp3
newdilp3 <- na.omit(dilp3qPCR)
#  making the dilp3 data long
long_dilp3 <- newdilp3 %>% 
  pivot_longer(cols = ("A1":"D3"), names_to = "sample", values_to = "cq")
view(long_dilp3)
# making a summary of dilp3
dilp3_summary <- long_dilp3 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))



rp20qPCR <- read_excel("data/qPCR_rp20.xlsx")

newrp20 <- na.omit(rp20qPCR)

long_rp20 <- newrp20 %>% 
  pivot_longer(cols = ("A1":"D3"), names_to = "sample", values_to = "cq")


rp20_summary <- long_rp20 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))




# average reference ct

avg_ref_ct <- rbind(newlong_fd38qPCR, long_rp20)

view(long_rp20)
view(newlong_fd38qPCR)
view(avg_ref_ct)

avg_ref_ct_summary <- avg_ref_ct %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))

is.na(foxoqPCR2)

newfoxoqpcr2 <- na.omit(foxoqPCR2)

newlong_foxoqPCR2 <- newfoxoqpcr2 %>% 
  pivot_longer(cols = ("A":"D"), names_to = "sample", values_to = "cq")


newfoxoqPCR2_summary <- newlong_foxoqPCR2 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n))


newfoxoqPCR_plot2 <- newfoxoqPCR2_summary %>% 
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

 
