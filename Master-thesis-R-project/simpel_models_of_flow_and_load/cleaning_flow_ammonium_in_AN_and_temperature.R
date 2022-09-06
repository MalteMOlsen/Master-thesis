#///////////////////////////////////////////////////////////////////////////////
#Finding values to use in data cleaning
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find values of which the flow to the AN tank
#the ammonium concentration to the AN tank and the temperature of the four 
#process tanks are considered to be measured wrongly. These values will be used
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
#-------------------------------------------------------------------------------
#Constants that will be used through this script
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Make a diff column for the half hourly data frame 
data_thirty_min <- data_thirty_min %>% 
  mutate(diff_flow_AN=difference(flow_AN_m3_h))%>% 
  mutate(diff_ammonium_AN=difference(ammonium_to_AN_mg_L))%>% 
  mutate(diff_T_PT4=difference(T_PT4_C))%>% 
  mutate(diff_T_PT3=difference(T_PT3_C))%>% 
  mutate(diff_T_PT2=difference(T_PT2_C))%>% 
  mutate(diff_T_PT1=difference(T_PT1_C))

#Make a diff column for the one minute data frame 
data_one_min <- data_one_min %>% 
  mutate(diff_flow_AN=difference(flow_AN_m3_h))%>% 
  mutate(diff_ammonium_AN=difference(ammonium_to_AN_mg_L))%>% 
  mutate(diff_T_PT4=difference(T_PT4_C))%>% 
  mutate(diff_T_PT3=difference(T_PT3_C))%>% 
  mutate(diff_T_PT2=difference(T_PT2_C))%>% 
  mutate(diff_T_PT1=difference(T_PT1_C))



#The next section is the creation of multiple graphs and the purpose is to 
#visualize the different time series in the attempt th make some meaningful 
#rule to clean data by

#Flow to the AN tank and difference of flow
#-------------------------------------------------------------------------------
#overall plot without any cleaning
#Flow to the AN tank
data_one_min %>% 
  select(flow_AN_m3_h) %>% 
  autoplot()

#Difference in flow to the An tank
data_one_min %>% 
  select(diff_flow_AN) %>% 
  autoplot()

#Plotting the difference in the flow to the AN tank in three different plots to 
#increase the resolution.
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


#Rain
#-------------------------------------------------------------------------------
#Plotting the rain in the half hourly aggregation
data_thirty_min %>% 
  select(rainfall_mm) %>% 
  autoplot()

#Plotting the rain data in the one minute fomate
data_one_min %>% 
  select(rainfall_mm) %>% 
  autoplot()


#Ammonium concentration to the AN tank
#-------------------------------------------------------------------------------
#Plotting the overall ammonium concentrations
data_one_min %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

#Plotting the ammonium concentration where all values over 100 mg/L
data_one_min %>% 
  filter(ammonium_to_AN_mg_L<=100) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

#Plotting the ammonium concentration where all values over 75 mg/L
data_one_min %>% 
  filter(ammonium_to_AN_mg_L<=75) %>% 
  select(ammonium_to_AN_mg_L) %>% 
  autoplot()

#Plotting the ammonium concentration in three different plot to increase the 
#resolution with the removal all values over 100 mg/L
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

#Plotting the ammonium concentration in three different plot to increase the 
#resolution with the removal all values over 75 mg/L
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

#Plotting the ammonium concentration in three different plot to increase the 
#resolution with the removal all values over 100 mg/L and values of 0 mg/L
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

#Plotting the difference in the ammonium concentration in three different plot 
#to increase the resolution with the removal all values over 100 mg/L and values 
#of 0 mg/L
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

#Plotting the difference in the ammonium concentration in three different plot 
#to increase the resolution with the removal all values over 100 mg/L and values 
#of 0 mg/L, and removing difference values higher or lower than 10 mg/L
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
#Plotting the over temperature in the process tank 4
data_one_min %>% 
  select(T_PT4_C) %>% 
  autoplot()

#Plotting the temperature in process tank 4 removing all values over 25 and all
#values which are zero
data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  select(diff_T_PT4) %>% 
  autoplot()

#Plotting the temperature difference in process tank 4 removing all values over 
#25 and all values which are zero, and all difference values over and under 5
data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  filter(diff_T_PT4<=5) %>% 
  filter(diff_T_PT4>=-5) %>% 
  select(diff_T_PT4) %>% 
  autoplot()

#Plotting the temperature difference in process tank 4 removing all values over 
#25 and all values which are zero, and all difference values over and under 2.5
data_one_min %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<=25) %>% 
  filter(diff_T_PT4<=2.5) %>% 
  filter(diff_T_PT4>=-2.5) %>% 
  select(diff_T_PT4) %>% 
  autoplot()


#Temperature in process tank 3
#-------------------------------------------------------------------------------
#Plotting the over temperature in the process tank 3
data_one_min %>% 
  select(T_PT3_C) %>% 
  autoplot()

#Plotting the temperature in process tank 3 removing all values over 25 and all
#values which are zero
data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  select(diff_T_PT3) %>% 
  autoplot()

#Plotting the temperature difference in process tank 3 removing all values over 
#25 and all values which are zero, and all difference values over and under 5
data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  filter(diff_T_PT3<=5) %>% 
  filter(diff_T_PT3>=-5) %>% 
  select(diff_T_PT3) %>% 
  autoplot()

#Plotting the temperature difference in process tank 3 removing all values over 
#25 and all values which are zero, and all difference values over and under 2.5
data_one_min %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<=25) %>% 
  filter(diff_T_PT3<=2.5) %>% 
  filter(diff_T_PT3>=-2.5) %>% 
  select(diff_T_PT3) %>% 
  autoplot()


#Temperature in process tank 2
#-------------------------------------------------------------------------------
#Plotting the over temperature in the process tank 2
data_one_min %>% 
  select(T_PT2_C) %>% 
  autoplot()

#Plotting the temperature in process tank 2 removing all values over 25 and all
#values which are zero
data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  select(diff_T_PT2) %>% 
  autoplot()

#Plotting the temperature difference in process tank 2 removing all values over 
#25 and all values which are zero, and all difference values over and under 5
data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  filter(diff_T_PT2<=5) %>% 
  filter(diff_T_PT2>=-5) %>% 
  select(diff_T_PT2) %>% 
  autoplot()

#Plotting the temperature difference in process tank 2 removing all values over 
#25 and all values which are zero, and all difference values over and under 2.5
data_one_min %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<=25) %>% 
  filter(diff_T_PT2<=2.5) %>% 
  filter(diff_T_PT2>=-2.5) %>% 
  select(diff_T_PT2) %>% 
  autoplot()


#Temperature in process tank 1
#-------------------------------------------------------------------------------
#Plotting the over temperature in the process tank 1
data_one_min %>% 
  select(T_PT1_C) %>% 
  autoplot()

#Plotting the temperature after removing all the values over 50
data_one_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C<=50) %>%
  autoplot()

#Plotting the temperature in process tank 1 removing all values over 25 and all
#values which are zero
data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  select(diff_T_PT1) %>% 
  autoplot()

#Plotting the temperature difference in process tank 1 removing all values over 
#25 and all values which are zero, and all difference values over and under 5
data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  filter(diff_T_PT1<=5) %>% 
  filter(diff_T_PT1>=-5) %>% 
  select(diff_T_PT1) %>% 
  autoplot()

#Plotting the temperature difference in process tank 1 removing all values over 
#25 and all values which are zero, and all difference values over and under 2.5
data_one_min %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<=25) %>% 
  filter(diff_T_PT1<=2.5) %>% 
  filter(diff_T_PT1>=-2.5) %>% 
  select(diff_T_PT1) %>% 
  autoplot()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Making the mean, quantile, and sd table
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Converting the data one minute data frame to a tibble
data_one_min <- data_one_min %>% 
  as_tibble()


#Flow
#------
#Finding the mean
mean(data_one_min$flow_AN_m3_h, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(flow_AN_m3_h) %>%
  quantile(na.rm = T)
  
#Finding the standard deviation
sd(data_one_min$flow_AN_m3_h, na.rm = T)


#Ammonium
#------
#Finding the mean
mean(data_one_min$ammonium_AN_mg_L, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(ammonium_AN_mg_L) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$ammonium_AN_mg_L, na.rm = T)

#Temperature process tank 4
#------
#Finding the mean
mean(data_one_min$T_PT4_C, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(T_PT4_C) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$T_PT4_C, na.rm = T)


#Temperature process tank 3
#------
#Finding the mean
mean(data_one_min$T_PT3_C, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(T_PT3_C) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$T_PT3_C, na.rm = T)


#Temperature process tank 2
#------
#Finding the mean
mean(data_one_min$T_PT2_C, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(T_PT2_C) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$T_PT2_C, na.rm = T)


#Temperature process tank 1
#------
#Finding the mean
mean(data_one_min$T_PT1_C, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(T_PT1_C) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$T_PT1_C, na.rm = T)


#Temperature process tank 1 (where values over 100 is removed)
#------
#Creating the temporary data frame which removing values over 100
temp <- data_one_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C<100)

#Finding the mean
mean(temp$T_PT1_C, na.rm = T)

#Finding the quantiles, min, and max
temp %>% 
  select(T_PT1_C) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(temp$T_PT1_C, na.rm = T)


#-------------------------------------------------------------------------------
#The diff values

#Difference flow
#-------
#Finding the mean
mean(data_one_min$diff_flow_AN, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(diff_flow_AN) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$diff_flow_AN, na.rm = T)


#Difference ammonium
#-------
#Finding the mean
mean(data_one_min$diff_ammonium_AN, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(diff_ammonium_AN) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$diff_ammonium_AN, na.rm = T)


#Temperature process tank 4
#-------
#Finding the mean
mean(data_one_min$diff_T_PT4, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(diff_T_PT4) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$diff_T_PT4, na.rm = T)


#Temperature process tank 3
#-------
#Finding the mean
mean(data_one_min$diff_T_PT3, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(diff_T_PT3) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$diff_T_PT3, na.rm = T)


#Temperature process tank 2
#-------
#Finding the mean
mean(data_one_min$diff_T_PT2, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(diff_T_PT2) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$diff_T_PT2, na.rm = T)


#Temperature process tank 1
#-------
#Finding the mean
mean(data_one_min$diff_T_PT1, na.rm = T)

#Finding the quantiles, min, and max
data_one_min %>% 
  select(diff_T_PT1) %>%
  quantile(na.rm = T)

#Finding the standard deviation
sd(data_one_min$diff_T_PT1, na.rm = T)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Making density and QQ plots to check for normal distribution
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

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

#Creating the qqplot for the different time series to check for normality

#Flow
qqPlot(check_for_normal_distribution$flow_AN_m3_h, 
       param.list = list(mean=1371.591, 
                         sd=766.0116))

#Diff flow
qqPlot(check_for_normal_distribution$diff_flow_AN, 
       param.list = list(mean=0.001086, 
                         sd=17.80615))

#Ammonium
qqPlot(check_for_normal_distribution$ammonium_to_AN_mg_L, 
       param.list = list(mean=27.6839, 
                         sd=18.95856))

#Diff ammonium
qqPlot(check_for_normal_distribution$diff_ammonium_AN, 
       param.list = list(mean=8.98*10^-6, 
                         sd=0.6139333))

#Temperature in process tank 1
qqPlot(check_for_normal_distribution$T_PT1_C, 
       param.list = list(mean=14.66281, 
                         sd=11.82106))

#Diff temperature in process tank 1
qqPlot(check_for_normal_distribution$diff_T_PT1, 
       param.list = list(mean=1.44*10^-5, 
                         sd=0.7578559))

#Temperature in process tank 2
qqPlot(check_for_normal_distribution$T_PT2_C, 
       param.list = list(mean=14.12814, 
                         sd=3.183633))

#Diff temperature in process tank 2
qqPlot(check_for_normal_distribution$diff_T_PT2, 
       param.list = list(mean=1.40*10^-5, 
                         sd=0.1006446))

#Temperature in process tank 3
qqPlot(check_for_normal_distribution$T_PT3_C, 
       param.list = list(mean=14.06911, 
                         sd=3.096116))

#Diff temperature in process tank 3
qqPlot(check_for_normal_distribution$diff_T_PT3, 
       param.list = list(mean=6.81*10^-6, 
                         sd=0.08982443))

#Temperature in process tank 4
qqPlot(check_for_normal_distribution$T_PT4_C, 
       param.list = list(mean=14.06802, 
                         sd=3.11735))

#Diff temperature in process tank 4
qqPlot(check_for_normal_distribution$diff_T_PT4, 
       param.list = list(mean=6.93*10^-6, 
                         sd=0.102309))