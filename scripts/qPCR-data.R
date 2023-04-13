library(tidyverse)
library(readxl)
library(kableExtra)
library(performance)
library(see)
library(patchwork)
library(usethis)
library(devtools)
library(knitr)
library(emmeans)
library(here)
library(sjPlot)
library(gtsummary)
library(knitr)
library(rphylopic)

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
  summarise(mean = mean(cq, na.rm = T))

# USE FROM HERE ------------

# reading the summarised foxo data in 
foxo_calcs <- read_excel("data/foxo_calcs.xlsx")

# making the summarised foxo data long 
newlong_foxo_calcs <- foxo_calcs %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")

# making a summary of the foxo data 
foxo_sum <- newlong_foxo_calcs%>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T),
            sd = sd(cq, na.rm = T),
            n = n(),
            se = sd/sqrt(n))


# foxo plot 
foxo_plot <- foxo_sum %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  theme_classic()+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  labs(title = "foxo",
       x = "Diet larvae were reared on \n(Protein: Carbohydrate/ Food Hardness)",
       y = "2^-ΔCt")+
  geom_jitter(data = newlong_foxo_calcs,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


# DATA ANALYSIS -----

# linear model 
qpcr_foxo_lm <- lm(cq ~ sample, data = newlong_foxo_calcs)

# performance check 
performance::check_model(qpcr_foxo_lm)
performance::check_model(qpcr_foxo_lm, check = c("qq"))
performance::check_model(qpcr_foxo_lm, check = c("linearity"))
performance::check_model(qpcr_foxo_lm, check = c("homogeneity"))

# summary for analysis 
summary(qpcr_foxo_lm)

# generalised linear model 
qpcr_foxo_glm <- glm(cq ~ sample, family = poisson, data = newlong_foxo_calcs)
# looks like it is overdispersed
# trying with quasipoisson 
qpcr_foxo_glm_2 <- glm(cq ~ sample, family = quasipoisson, data = newlong_foxo_calcs)

# performance check 
performance::check_model(qpcr_foxo_glm_2)
performance::check_model(qpcr_foxo_glm_2, check = c("qq"))

# summary for analysis 
summary(qpcr_foxo_glm_2)

# tukey test with emmeans 
emmeans::emmeans(qpcr_foxo_glm_2, pairwise ~ sample)



# reading the summarised dilp3 data in 
dilp3_calcs <- read_excel("data/dilp3_calcs.xlsx")

# making the  data long 
newlong_dilp3_calcs <- dilp3_calcs %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")
# making the data long summary
dilp3_sum <- newlong_dilp3_calcs %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T),
sd = sd(cq, na.rm = T),
n = n(),
se = sd/sqrt(n))
# dilp3 plot 
dilp3_plot <- dilp3_sum %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  theme_classic()+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  labs(title = "dilp3",
       x = "Diet larvae were reared on \n(Protein: Carbohydrate/ Food Hardness)",
       y = "2^-ΔCt")+
  geom_jitter(data = newlong_dilp3_calcs,
              aes(x = sample,
                  y = cq),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)





# binding the foxo and dilp3 calcs together
foxo_dilp3_calcs <- rbind(newlong_foxo_calcs, newlong_dilp3_calcs)

# making a summary of the foxo and dilp 3 calcs 
foxo_dilp3_summary <- foxo_dilp3_calcs %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T),
            sd = sd(cq, na.rm = T),
            n = n(),
            se = sd/sqrt(n))






# patchwork to look at foxo and dilp3 together 
foxo_plot + dilp3_plot


foxo_dilp3_plot <- foxo_dilp3_summary %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#cc79a7",
           alpha = 0.6)+
  theme_classic()+
  labs(x = "Protein: Carbohydrate Diet larvae were reared on",
       y = "2^-ΔCt")+


# data analysis of qPCR results 
qpcr_results_lm <- lm(cq ~ sample, data = foxo_dilp3_calcs)

# checking the model
performance::check_model(qpcr_results_lm)
performance::check_model(qpcr_results_lm, check = c("qq"))
performance::check_model(qpcr_results_lm, check = c("linearity"))
performance::check_model(qpcr_results_lm, check = c("homogeneity"))

# trying a glm 
qpcr_results_glm <- glm(cq ~ sample, family = quasipoisson, data = newnew)

# checking the model
performance::check_model(qpcr_results_glm)
performance::check_model(qpcr_results_glm, check = c("qq"))
performance::check_model(qpcr_results_glm, check = c("homogeneity"))
# qq looks better 

# summary function to look at results 
summary(qpcr_results_glm)

# tukey test
emmeans::emmeans(qpcr_results_glm, pairwise ~ sample)



# reading the summarised dilp3 data in 
dilp3_foxo_new_calcs <- read_excel("data/calculations_qPCR.xlsx")

# making the  data long 
dilp3_foxo_new_calcs_long <- dilp3_foxo_new_calcs %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")
# making the data long summary
dilp3_foxo_new_calcs_summary <- dilp3_foxo_new_calcs_long %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T))




dilp3_foxo_new_calcs_plot <- dilp3_foxo_new_calcs_summary %>% 
  ggplot(aes(x = sample, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#cc79a7",
           alpha = 0.6)+
  theme_classic()+
  labs(x = "Protein: Carbohydrate Diet larvae were reared on",
       y = "2^-ΔCt")


qpcr_foxo_lm <- lm(cq ~ sample, data = newlong_foxo_calcs)

summary(qpcr_foxo_lm)

qpcr_dilp3_lm <- lm(cq ~ sample, data = newlong_dilp3_calcs)

summary(qpcr_dilp3_lm)

# data analysis of qPCR results 
qpcr_results_new_lm <- lm(cq ~ sample, data = dilp3_foxo_new_calcs_long)
qpcr_results_new_glm <- glm(cq ~ sample, family = quasipoisson(), data = dilp3_foxo_new_calcs_long)

performance::check_model(qpcr_results_new_glm)


# checking the model
performance::check_model(qpcr_results_new_lm)
performance::check_model(qpcr_results_new_lm , check = c("qq"))
performance::check_model(qpcr_results_lm, check = c("linearity"))
performance::check_model(qpcr_results_lm, check = c("homogeneity"))

summary(qpcr_results_new_lm)



AIC(qpcr_results_new_lm)



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
  summarise(mean = mean(cq))

both_summary <- rbind(dilp3_summary, foxoqPCR2_summary)

dilp3_foxo_calcs_summary <- both_summary %>%  
  group_by(sample) %>% 
  summarise(mean = mean(mean))

(6670 - 6600) 

# (the average of dilp3 and foxo) - (the average of fd38 and rp20) ---- 
#A1
(34.4 - 27.7)
6.7
2^- 6.7
0.009618316
#A2
(34.0 - 24.8)
9.2
2^- 9.2
0.001700294
#A3
(37 -  30.8)
6.2
2^-6.2
0.01360235
#B1
(35.1 -  25.2)
9.9 
2^-9.9
0.001046654
#B2
(28.0  - 24.3 )
3.7
2^-3.7
0.07694653
#B3
(34.0 - 26.4)
7.6
2^-7.6
0.005154328
#C1
( 35.6 -  27.1)
8.5
2^-8.5
0.002762136
#C2
(34.9-26.4)
8.5
2^-8.5
0.002762136
#C3
( 35.0 -  27.7 )
7.3
2^- 7.3
0.006345722
#D2
(35.2- 27.3)
7.9
2^- 7.9
0.004186615




#------------------------------------------
# binding the fd38 and rp20 data summaries 
fd38_rp20 <- rbind(newfd38qPCR_summary2, rp20_summary)
# summary of fd38 and rp20 together 
fd38_rp20_summary <- fd38_rp20 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(mean))

7018 - 6600

# dilp3 ---- 
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




qPCR_plot2 <- qPCR_data %>% 
  ggplot(aes(x = sample, y = Cq))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  theme_classic()


view(qPCR_data)




2^- 6.65



qPCR_over <- read_excel("data/qPCR_set.xlsx")

long_qPCR <- qPCR_over %>% 
  pivot_longer(cols = ("1:8S(1)":"8:1H(2)"), names_to = "sample", values_to = "cq")

qPCR_summary <- read_excel("data/qPCR_summary_3.xlsx")


long_qPCR_sum <- qPCR_summary %>% 
  pivot_longer(cols = ("1:8S":"8:1H"), names_to = "sample", values_to = "cq")

# foxo
#A1
(34.5 - 27.7)
6.8
2^- 6.8
0.008974206
#A2
(32.6 - 24.8)
7.8
2^- 7.8
0.004487103
#A3
(39.4 -  30.8)
8.6
2^-8.6
0.002577164
#B1
(34.7 -  25.2)
9.5
2^-9.5
0.002577164
#B2
(29.3  - 24.3 )
5
2^-5 
0.03125
#B3
(32.8 - 26.4)
6.4
2^-6.4
0.01184154
#C1
( 36.0 -  27.1)
8.9
2^-8.9
0.002093308
#C2
(36.0 -26.4)
9.6
2^-9.6
0.001288582
#C3
( 35.8 -  27.7 )
8.1
2^- 8.1
0.00364466
#D2
(34.9- 27.3)
7.6
2^- 7.6
0.005154328

sample <- c("1:8S(1)","1:8S(2)", "1:8S(3)", "1:8H(1)","1:8H(2)", "1:8H(3)", "8:1S(1)", "8:1S(2)", "8:1S(3)", "8:1H(2)")
Cq <- c("0.008974206", "0.004487103", "0.002577164", "0.002577164", "0.03125", "0.01184154", "0.002093308", "0.001288582", " 0.00364466", "0.005154328"  )

foxo_bind <- data.frame(sample, Cq)

qPCR_plot3 <- foxo_bind %>% 
  ggplot(aes(x = sample, y = Cq))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  theme_classic()

qPCR_plot4 <- bothqpcr %>% 
  ggplot(aes(x = sample, y = Cq))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  theme_classic()



qPCR_plot2 + qPCR_plot3

bothqpcr <- rbind(qPCR_data, foxo_bind)

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








foxoqPCR2 <- read_excel("data/qPCR_foxo_data_2.xlsx", na = "NA")


long_foxoqPCR2 <- foxoqPCR2 %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")



foxoqPCR2_summary <- long_foxoqPCR2 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T))
            


  

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

 
