#///////////////////////////////////////////////////////////////////////////////
#Finding values to use in data cleaning
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find values of which the flow to the AN tank
#the ammonium concentration to the AN tank and the temperature of the four 
#process tanks are considered to be measuring wrongly. These values will be used
#to create strict rule, which will be applied to all data.


#Setup
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load all the data
source("load_all_data.R")

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")


#-------------------------------------------------------------------------------
#Constants that will be used through this script
year="2021"
month="2021-11"
timeperiod="2021-08"~"2021-12"
zoom="2021-11-29"~"2021-11-30"

#make a diff column
data_thirty_min <- data_thirty_min %>% 
  mutate(diff_flow_AN=difference(flow_AN_m3_h))%>% 
  mutate(diff_ammonium_AN=difference(ammonium_to_AN_mg_L))%>% 
  mutate(diff_T_PT4=difference(T_PT4_C))%>% 
  mutate(diff_T_PT3=difference(T_PT3_C))%>% 
  mutate(diff_T_PT2=difference(T_PT2_C))%>% 
  mutate(diff_T_PT1=difference(T_PT1_C))


data_one_min <- data_one_min %>% 
  mutate(diff_flow_AN=difference(flow_AN_m3_h))%>% 
  mutate(diff_ammonium_AN=difference(ammonium_to_AN_mg_L))%>% 
  mutate(diff_T_PT4=difference(T_PT4_C))%>% 
  mutate(diff_T_PT3=difference(T_PT3_C))%>% 
  mutate(diff_T_PT2=difference(T_PT2_C))%>% 
  mutate(diff_T_PT1=difference(T_PT1_C))
#Flow to the AN tank
#-------------------------------------------------------------------------------
#overall

data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()


data_thirty_min %>% 
  select(diff_flow_AN) %>% 
  autoplot()


data_one_min %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()


data_one_min %>% 
  select(diff_flow_AN) %>% 
  autoplot()


temp <- data_one_min %>% 
  filter(flow_AN_m3_h==0)
temp <- data_thirty_min %>% 
  filter(flow_AN_m3_h==0)




p1 <- data_one_min %>% 
  filter_index("2018-01-01"~"2019-08-01") %>% 
  select(diff_flow_AN) %>% 
  autoplot()
p2 <- data_one_min %>%
  filter_index("2019-08-01"~"2021-01-01") %>% 
  select(diff_flow_AN) %>% 
  autoplot()
p3 <- data_one_min %>% 
  filter_index("2021-01-01"~.) %>% 
  select(diff_flow_AN) %>% 
  autoplot()

gridExtra::grid.arrange(p1,p2,p3)



temp <- data_one_min %>% 
  filter(diff_flow_AN>250)%>% 
  filter(diff_flow_AN<-250)

temp1 <- data_one_min %>% 
  filter(diff_flow_AN>250 )

temp2 <- data_one_min %>%
  filter(diff_flow_AN<=-250)



#Rain
#-------------------------------------------------------------------------------
data_thirty_min %>% 
  select(rainfall_mm) %>% 
  autoplot()

data_one_min %>% 
  select(rainfall_mm) %>% 
  autoplot()

#Ammonium concentration to the AN tank
#-------------------------------------------------------------------------------
data_one_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

data_one_min %>% 
  filter(ammonium_to_AN_mg_L<=100) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

data_one_min %>% 
  filter(ammonium_to_AN_mg_L<=75) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

data_thirty_min %>% 
  select(ammonium_AN_mg_L) %>% 
  autoplot()

data_thirty_min %>% 
  filter(ammonium_to_AN_mg_L<250) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()



p1 <- data_one_min %>% 
  filter_index("2018-01-01"~"2019-08-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
p2 <- data_one_min %>%
  filter_index("2019-08-01"~"2021-01-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
p3 <- data_one_min %>% 
  filter_index("2021-01-01"~.) %>% 
  filter(ammonium_to_AN_mg_L<=100) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

gridExtra::grid.arrange(p1,p2,p3)






p1 <- data_one_min %>% 
  filter_index("2018-01-01"~"2019-08-01") %>% 
  filter(ammonium_to_AN_mg_L<=75) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
p2 <- data_one_min %>%
  filter_index("2019-08-01"~"2021-01-01") %>% 
  filter(ammonium_to_AN_mg_L<=75) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
p3 <- data_one_min %>% 
  filter_index("2021-01-01"~.) %>% 
  filter(ammonium_to_AN_mg_L<=75) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

gridExtra::grid.arrange(p1,p2,p3)





p1 <- data_one_min %>% 
  filter_index("2018-01-01"~"2019-08-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
p2 <- data_one_min %>%
  filter_index("2019-08-01"~"2021-01-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()
p3 <- data_one_min %>% 
  filter_index("2021-01-01"~.) %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

gridExtra::grid.arrange(p1,p2,p3)



p1 <- data_one_min %>% 
  filter_index("2018-01-01"~"2019-08-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  select(diff_ammonium_AN) %>% 
  autoplot()
p2 <- data_one_min %>%
  filter_index("2019-08-01"~"2021-01-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  select(diff_ammonium_AN) %>% 
  autoplot()
p3 <- data_one_min %>% 
  filter_index("2021-01-01"~.) %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  select(diff_ammonium_AN) %>% 
  autoplot()

gridExtra::grid.arrange(p1,p2,p3)



p1 <- data_one_min %>% 
  filter_index("2018-01-01"~"2019-08-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  filter(diff_ammonium_AN>-10) %>% 
  filter(diff_ammonium_AN<10) %>% 
  select(diff_ammonium_AN) %>% 
  autoplot()
p2 <- data_one_min %>%
  filter_index("2019-08-01"~"2021-01-01") %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  filter(diff_ammonium_AN>-10) %>% 
  filter(diff_ammonium_AN<10) %>% 
  select(diff_ammonium_AN) %>% 
  autoplot()
p3 <- data_one_min %>% 
  filter_index("2021-01-01"~.) %>% 
  filter(ammonium_to_AN_mg_L<=100) %>%
  filter(ammonium_to_AN_mg_L>0) %>%
  filter(diff_ammonium_AN>-10) %>% 
  filter(diff_ammonium_AN<10) %>% 
  select(diff_ammonium_AN) %>% 
  autoplot()

gridExtra::grid.arrange(p1,p2,p3)


#Temperature in process tank 4
#-------------------------------------------------------------------------------
#overall

data_thirty_min %>% 
  select(T_PT4_C) %>% 
  autoplot()


data_thirty_min %>% 
  select(diff_T_PT4) %>% 
  autoplot()

data_one_min %>% 
  select(T_PT4_C) %>% 
  autoplot()

data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  select(diff_T_PT4) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  filter(diff_T_PT4<=5) %>% 
  filter(diff_T_PT4>=-5) %>% 
  select(diff_T_PT4) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  filter(diff_T_PT4<=2.5) %>% 
  filter(diff_T_PT4>=-2.5) %>% 
  select(diff_T_PT4) %>% 
  autoplot()


temp <- data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  filter(diff_T_PT4<=2.5) %>% 
  filter(diff_T_PT4>=-2.5) 
#Temperature in process tank 3
#-------------------------------------------------------------------------------
#overall
data_one_min %>% 
  select(T_PT3_C) %>% 
  autoplot()

data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  select(diff_T_PT3) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  filter(diff_T_PT3<=5) %>% 
  filter(diff_T_PT3>=-5) %>% 
  select(diff_T_PT3) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  filter(diff_T_PT3<=2.5) %>% 
  filter(diff_T_PT3>=-2.5) %>% 
  select(diff_T_PT3) %>% 
  autoplot()


temp <- data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  filter(diff_T_PT3<=2.5) %>% 
  filter(diff_T_PT3>=-2.5)


#Temperature in process tank 2
#-------------------------------------------------------------------------------
#overall
data_one_min %>% 
  select(T_PT2_C) %>% 
  autoplot()

data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  select(diff_T_PT2) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  filter(diff_T_PT2<=5) %>% 
  filter(diff_T_PT2>=-5) %>% 
  select(diff_T_PT2) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  filter(diff_T_PT2<=2.5) %>% 
  filter(diff_T_PT2>=-2.5) %>% 
  select(diff_T_PT2) %>% 
  autoplot()


temp <- data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  filter(diff_T_PT2<=2.5) %>% 
  filter(diff_T_PT2>=-2.5)



#Temperature in process tank 1
#-------------------------------------------------------------------------------
#overall
data_one_min %>% 
  select(T_PT1_C) %>% 
  autoplot()

data_one_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C<=50) %>%
  autoplot()



data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  select(diff_T_PT1) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  filter(diff_T_PT1<=5) %>% 
  filter(diff_T_PT1>=-5) %>% 
  select(diff_T_PT1) %>% 
  autoplot()


data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  filter(diff_T_PT1<=2.5) %>% 
  filter(diff_T_PT1>=-2.5) %>% 
  select(diff_T_PT1) %>% 
  autoplot()


temp <- data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  filter(diff_T_PT1<=2.5) %>% 
  filter(diff_T_PT1>=-2.5)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Making the mean, quantile, and sd table
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
data_one_min <- data_one_min %>% 
  as_tibble()


#Flow
mean(data_one_min$flow_AN_m3_h, na.rm = T)

data_one_min %>% 
  select(flow_AN_m3_h) %>%
  quantile(na.rm = T)
  
sd(data_one_min$flow_AN_m3_h, na.rm = T)


#Ammonium
mean(data_one_min$ammonium_AN_mg_L, na.rm = T)

data_one_min %>% 
  select(ammonium_AN_mg_L) %>%
  quantile(na.rm = T)

sd(data_one_min$ammonium_AN_mg_L, na.rm = T)

#Temperature process tank 4
mean(data_one_min$T_PT4_C, na.rm = T)

data_one_min %>% 
  select(T_PT4_C) %>%
  quantile(na.rm = T)

sd(data_one_min$T_PT4_C, na.rm = T)


#Temperature process tank 3
mean(data_one_min$T_PT3_C, na.rm = T)

data_one_min %>% 
  select(T_PT3_C) %>%
  quantile(na.rm = T)

sd(data_one_min$T_PT3_C, na.rm = T)


#Temperature process tank 2
mean(data_one_min$T_PT2_C, na.rm = T)

data_one_min %>% 
  select(T_PT2_C) %>%
  quantile(na.rm = T)

sd(data_one_min$T_PT2_C, na.rm = T)


#Temperature process tank 1
mean(data_one_min$T_PT1_C, na.rm = T)

data_one_min %>% 
  select(T_PT1_C) %>%
  quantile(na.rm = T)

sd(data_one_min$T_PT1_C, na.rm = T)


#Temperature process tank 1 (where values over 100 is removed)
temp <- data_one_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C<100)

mean(temp$T_PT1_C, na.rm = T)

temp %>% 
  select(T_PT1_C) %>%
  quantile(na.rm = T)

sd(temp$T_PT1_C, na.rm = T)


#----
#The diff values

#Difference flow
mean(data_one_min$diff_flow_AN, na.rm = T)

data_one_min %>% 
  select(diff_flow_AN) %>%
  quantile(na.rm = T)

sd(data_one_min$diff_flow_AN, na.rm = T)


#Difference ammonium
mean(data_one_min$diff_ammonium_AN, na.rm = T)

data_one_min %>% 
  select(diff_ammonium_AN) %>%
  quantile(na.rm = T)

sd(data_one_min$diff_ammonium_AN, na.rm = T)


#Temperature process tank 4
mean(data_one_min$diff_T_PT4, na.rm = T)

data_one_min %>% 
  select(diff_T_PT4) %>%
  quantile(na.rm = T)

sd(data_one_min$diff_T_PT4, na.rm = T)


#Temperature process tank 3
mean(data_one_min$diff_T_PT3, na.rm = T)

data_one_min %>% 
  select(diff_T_PT3) %>%
  quantile(na.rm = T)

sd(data_one_min$diff_T_PT3, na.rm = T)


#Temperature process tank 2
mean(data_one_min$diff_T_PT2, na.rm = T)

data_one_min %>% 
  select(diff_T_PT2) %>%
  quantile(na.rm = T)

sd(data_one_min$diff_T_PT2, na.rm = T)


#Temperature process tank 1
mean(data_one_min$diff_T_PT1, na.rm = T)

data_one_min %>% 
  select(diff_T_PT1) %>%
  quantile(na.rm = T)

sd(data_one_min$diff_T_PT1, na.rm = T)


#------
#Making density and QQ plots to check for normal distribution

check_for_normal_distribution <- data_one_min %>% 
  select(flow_AN_m3_h, 
         diff_flow_AN, 
         ammonium_to_AN_mg_L,
         diff_ammonium_AN,
         T_PT1_C,
         T_PT2_C,
         T_PT3_C,
         T_PT4_C,
         diff_T_PT1,
         diff_T_PT2,
         diff_T_PT3,
         diff_T_PT4) %>% 
  na.omit()

#FLow
check_for_normal_distribution %>% 
  select(flow_AN_m3_h) %>%
  ggplot(aes(flow_AN_m3_h))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, c(mean = mean(flow_AN_m3_h), sd = sd(flow_AN_m3_h))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$flow_AN_m3_h, 
       param.list = list(mean=1371.591, 
                         sd=766.0116))

#Diff flow
check_for_normal_distribution %>% 
  select(diff_flow_AN) %>%
  ggplot(aes(diff_flow_AN))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(diff_flow_AN), 
                  sd = sd(diff_flow_AN))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$diff_flow_AN, 
       param.list = list(mean=0.001086, 
                         sd=17.80615))

#Ammonium
check_for_normal_distribution %>% 
  select(ammonium_to_AN_mg_L) %>%
  ggplot(aes(ammonium_to_AN_mg_L))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(ammonium_to_AN_mg_L), 
                  sd = sd(ammonium_to_AN_mg_L))),
    color="Orange"
  )

qqPlot(check_for_normal_distribution$ammonium_to_AN_mg_L, 
       param.list = list(mean=27.6839, 
                         sd=18.95856))

#Diff ammonium
check_for_normal_distribution %>% 
  select(diff_ammonium_AN) %>%
  ggplot(aes(diff_ammonium_AN))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(diff_ammonium_AN), 
                  sd = sd(diff_ammonium_AN))),
    color="Orange"
  )

qqPlot(check_for_normal_distribution$diff_ammonium_AN, 
       param.list = list(mean=8.98*10^-6, 
                         sd=0.6139333))


#Temperature in process tank 1
check_for_normal_distribution %>% 
  select(T_PT1_C) %>%
  ggplot(aes(T_PT1_C))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(T_PT1_C), 
                  sd = sd(T_PT1_C))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$T_PT1_C, 
       param.list = list(mean=14.66281, 
                         sd=11.82106))

#Diff temperature in process tank 1
check_for_normal_distribution %>% 
  select(diff_T_PT1) %>%
  ggplot(aes(diff_T_PT1))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(diff_T_PT1), 
                  sd = sd(diff_T_PT1))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$diff_T_PT1, 
       param.list = list(mean=1.44*10^-5, 
                         sd=0.7578559))




#Temperature in process tank 2
check_for_normal_distribution %>% 
  select(T_PT2_C) %>%
  ggplot(aes(T_PT2_C))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(T_PT2_C), 
                  sd = sd(T_PT2_C))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$T_PT2_C, 
       param.list = list(mean=14.12814, 
                         sd=3.183633))

#Diff temperature in process tank 2
check_for_normal_distribution %>% 
  select(diff_T_PT2) %>%
  ggplot(aes(diff_T_PT2))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(diff_T_PT2), 
                  sd = sd(diff_T_PT2))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$diff_T_PT2, 
       param.list = list(mean=1.40*10^-5, 
                         sd=0.1006446))



#Temperature in process tank 3
check_for_normal_distribution %>% 
  select(T_PT3_C) %>%
  ggplot(aes(T_PT3_C))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(T_PT3_C), 
                  sd = sd(T_PT3_C))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$T_PT3_C, 
       param.list = list(mean=14.06911, 
                         sd=3.096116))




#Diff temperature in process tank 3
check_for_normal_distribution %>% 
  select(diff_T_PT3) %>%
  ggplot(aes(diff_T_PT3))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(diff_T_PT3), 
                  sd = sd(diff_T_PT3))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$diff_T_PT3, 
       param.list = list(mean=6.81*10^-6, 
                         sd=0.08982443))


#Temperature in process tank 4
check_for_normal_distribution %>% 
  select(T_PT4_C) %>%
  ggplot(aes(T_PT4_C))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(T_PT4_C), 
                  sd = sd(T_PT4_C))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$T_PT4_C, 
       param.list = list(mean=14.06802, 
                         sd=3.11735))




#Diff temperature in process tank 4
check_for_normal_distribution %>% 
  select(diff_T_PT4) %>%
  ggplot(aes(diff_T_PT4))+
  geom_density()+
  stat_function(
    fun = dnorm,
    args = with(check_for_normal_distribution, 
                c(mean = mean(diff_T_PT4), 
                  sd = sd(diff_T_PT4))),
    color="Orange"
  )


qqPlot(check_for_normal_distribution$diff_T_PT4, 
       param.list = list(mean=6.93*10^-6, 
                         sd=0.102309))
