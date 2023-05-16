

#  reading the dilp3 data in
dilp3qPCR <- read_excel("data/qPCR_dilp3.xlsx")
#  making the dilp3 data long
long_dilp3 <- dilp3qPCR %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
view(long_dilp3)
# making a summary of dilp3
dilp3_summary <- long_dilp3 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq))

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
#------------------------------------------
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
# reading the rp20 data in
rp20qPCR <- read_excel("data/qPCR_rp20.xlsx")
# making the data long
long_rp20 <- rp20qPCR %>% 
  pivot_longer(cols = ("A1":"D2"), names_to = "sample", values_to = "cq")
# making a summary of the data 
rp20_summary <- long_rp20 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(cq, na.rm = T))
# binding the fd38 and rp20 data summaries 
fd38_rp20 <- rbind(newfd38qPCR_summary2, rp20_summary)
# summary of fd38 and rp20 together 
fd38_rp20_summary <- fd38_rp20 %>%  
  group_by(sample) %>% 
  summarise(mean = mean(mean))


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
