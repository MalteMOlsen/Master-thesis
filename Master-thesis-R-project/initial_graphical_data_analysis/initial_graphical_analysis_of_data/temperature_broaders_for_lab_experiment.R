#///////////////////////////////////////////////////////////////////////////////
#Finding the temperature broaders and quantiles
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to find the broaders of temperature and which to 
#include in the investigation.

#Setup
#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load all the data
source("load_all_data.R")

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")


#Making a plot of the four temperature column

#Process tank 4
#--------
data_thirty_min %>% 
  select(T_PT4_C) %>% 
  autoplot()+
  theme_malte()

data_thirty_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  autoplot()+
  theme_malte()

data_one_min %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  ggplot(aes(x=T_PT4_C))+
  geom_density()+
  xlab("Temperature in process tank 4")+
  theme_malte()
    

data_one_min %>%
  as_tibble() %>% 
  select(T_PT4_C) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)

data_one_min %>%
  as_tibble() %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)

tempPT4 <- data_one_min %>%
  as_tibble() %>% 
  select(T_PT4_C) %>% 
  filter(T_PT4_C>0) %>% 
  filter(T_PT4_C<25) 

mean(tempPT4$T_PT4_C, na.rm=T)

#Process tank 3
#--------
data_thirty_min %>% 
  select(T_PT3_C) %>% 
  autoplot()+
  theme_malte()

data_thirty_min %>% 
  select(T_PT3_C) %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<25) %>% 
  autoplot()+
  theme_malte()

data_one_min %>% 
  select(T_PT3_C) %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<25) %>% 
  ggplot(aes(x=T_PT3_C))+
  geom_density()+
  xlab("Temperature in process tank 3")+
  theme_malte()


data_one_min %>%
  as_tibble() %>% 
  select(T_PT3_C) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)

data_one_min %>%
  as_tibble() %>% 
  select(T_PT3_C) %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<25) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)

tempPT3 <- data_one_min %>%
  as_tibble() %>% 
  select(T_PT3_C) %>% 
  filter(T_PT3_C>0) %>% 
  filter(T_PT3_C<25) 

mean(tempPT3$T_PT3_C, na.rm=T)


#Process tank 2
#--------
data_thirty_min %>% 
  select(T_PT2_C) %>% 
  autoplot()+
  theme_malte()

data_thirty_min %>% 
  select(T_PT2_C) %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<25) %>% 
  autoplot()+
  theme_malte()

data_one_min %>% 
  select(T_PT2_C) %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<25) %>% 
  ggplot(aes(x=T_PT2_C))+
  geom_density()+
  xlab("Temperature in process tank 2")+
  theme_malte()


data_one_min %>%
  as_tibble() %>% 
  select(T_PT2_C) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)

data_one_min %>%
  as_tibble() %>% 
  select(T_PT2_C) %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<25) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)


tempPT2 <- data_one_min %>%
  as_tibble() %>% 
  select(T_PT2_C) %>% 
  filter(T_PT2_C>0) %>% 
  filter(T_PT2_C<25) 

mean(tempPT2$T_PT2_C, na.rm=T)

#Process tank 1
#--------
data_thirty_min %>% 
  select(T_PT1_C) %>% 
  autoplot()+
  theme_malte()

data_thirty_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<25) %>% 
  autoplot()+
  theme_malte()

data_one_min %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<25) %>% 
  ggplot(aes(x=T_PT1_C))+
  geom_density()+
  xlab("Temperature in process tank 1")+
  theme_malte()+
  geom_vline(xintercept = 11.5, colour="Red")


data_one_min %>%
  as_tibble() %>% 
  select(T_PT1_C) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)

data_one_min %>%
  as_tibble() %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<25) %>% 
  quantile(c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1),
           na.rm=T)


tempPT1 <- data_one_min %>%
  as_tibble() %>% 
  select(T_PT1_C) %>% 
  filter(T_PT1_C>0) %>% 
  filter(T_PT1_C<25) 

mean(tempPT1$T_PT1_C, na.rm=T)
