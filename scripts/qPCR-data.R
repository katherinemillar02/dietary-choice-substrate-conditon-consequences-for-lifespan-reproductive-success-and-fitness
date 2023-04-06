

# reading the foxo data in
foxoqPCR <- read_excel("data/qPCR_foxo_data_2.xlsx")
# removing the na values from the foxo data and changing the file name 
#newfoxoqpcr <- na.omit(foxoqPCR)
#view(newfoxoqpcr)
# making the new foxo data long 
newlong_foxoqPCR <- foxoqPCR %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
view(newlong_foxoqPCR)
#  making a summary of the foxo data with na values removed 
newfoxoqPCR_summary <- newlong_foxoqPCR %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n), na.rm = T)







#  reading the fd38 data in 
fd38qPCR <- read_excel("data/fd38_qPCR.xlsx")
# making the fd38 data long
newlong_fd38qPCR <- fd38qPCR %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
view(newlong_fd38qPCR)
# making a summary of the fd38 data
newfd38qPCR_summary <- newlong_fd38qPCR %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T))
# sd = sd(cq, na.rm = T),
#           n = n(),
#          se = sd/sqrt(n))
#------------------------------------------
# reading the rp20 data in
rp20qPCR <- read_excel("data/qPCR_rp20.xlsx")
# making the data long
long_rp20 <- rp20qPCR %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
# making a summary of the data 
rp20_summary <- long_rp20 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T))
# sd = sd(cq),
# n=n())
#           se = sd/sqrt(n), na.rm = T)
#------------------------------------------
# binding the fd38 and rp20 data summaries 
fd38_rp20 <- rbind(newfd38qPCR_summary2, rp20_summary)
# summary of fd38 and rp20 together 
fd38_rp20_summary <- fd38_rp20 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(mean))

#A1
(34.2 - 27.7)
6.5
2^- 6.5
0.01104854
#A2
(35.4 - 24.8)
10.6 
2^- 10.6
0.000644291
#A3
(34.6 -  30.8)
3.8
2^-3.8
0.07179365
#B1
(35.5 -  25.2)
10.3 
2^-10.3 
0.0007932152
#B2
(26.8  - 24.3 )
2.5 
2^-2.5 
0.1767767
#B3
(35.2 - 26.4)
8.8
2^-8.8
0.002243551
#C1
( 35.3 -  27.1)
8.2
2^-8.2
0.003400588
#C2
(33.7-26.4)
7.3
2^-7.3
0.006345722
#C3
( 34.2 -  27.7 )
 6.5
 2^- 6.5
 0.01104854
 #D2
(35.5- 27.3)
 8.2
 2^- 8.2
 0.003400588
 
 
sample <- c("1:8S(1)","1:8S(2)", "1:8S(3)", "1:8H(1)","1:8H(2)", "1:8H(3)", "8:1S(1)", "8:1S(2)", "8:1S(3)", "8:1H(2)")
Cq <- c("0.01104854", "0.000644291", "0.07179365", "0.0007932152", "0.1767767", "0.002243551", "0.003400588", "0.006345722", " 0.01104854", " 0.003400588"  )
 
qPCR_data <- data.frame(sample, Cq)


foxoqPCR_plot2 <- qPCR_data %>% 
  ggplot(aes(x = sample, y = Cq))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  theme_classic()


view(qPCR_data)








qPCR_over <- read_excel("data/qPCR_set.xlsx")

long_qPCR <- qPCR_over %>% 
  pivot_longer(cols = ("1:8S(1)":"8:1H(2)"), names_to = "sample", values_to = "cq")

qPCR_summary <- read_excel("data/qPCR_summary_3.xlsx")


long_qPCR_sum <- qPCR_summary %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")






 #  reading the dilp3 data in
dilp3qPCR <- read_excel("data/qPCR_dilp3.xlsx")
# removing na values from dilp3
# newdilp3 <- na.omit(dilp3qPCR)
#  making the dilp3 data long
long_dilp3 <- dilp3qPCR %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
view(long_dilp3)
# making a summary of dilp3
dilp3_summary <- long_dilp3 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq),
            sd = sd(cq),
            n = n(),
            se = sd/sqrt(n), na.rm = T)








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

 
