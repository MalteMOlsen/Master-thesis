#///////////////////////////////////////////////////////////////////////////////
#Laboratory results
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to visulize and analysize the laboratory results.

#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#-------------------------------------------------------------------------------
#Load the packages
source("load_packages.R")


#Defining the theme
theme_malte <- function(){
  theme_light()+
    theme(
      axis.line = element_line(),
      panel.border = element_blank())
}

#-------------------------------------------------------------------------------
#Write in the results

sample_time <- c(0,0,0,5,5,5,10,10,10,15,15,15,20,20,20,30,30,30,40,40,40,
                 50,50,50,60,60,60,70,70,70,80,80,80,95,95,95,110,110,110,
                 125,125,125,140,140,140)
ammonium_values <- c(22.1,21.3,21.2,
                     20.9,21.6,21.8,
                     21.2,20.8,22.3,
                     19.7,22.5,21.9,
                     18.8,18.5,18.4,
                     17.0,16.9,16.3,
                     16.6,17.7,18.3,
                     16.2,17.4,15.9,
                     15.6,15.0,15.8,
                     12.4,13.4,12.4,
                     14.3,14.3,13.5,
                     12.1,12.1,9.90,
                     10.4,11.2,10.7,
                     9.00,9.35,9.35,
                     7.65,7.89,8.18)
lab_data <- data.frame(sample_time,ammonium_values) %>% 
  group_by(sample_time) %>% 
  mutate(average_ammonium=mean(ammonium_values))



reg_data <- lab_data %>% 
  filter(sample_time>=15)
  

linear_reg_all_measurements <- lm(ammonium_values~sample_time,
                                  data=reg_data)
summary(linear_reg_all_measurements)


linear_reg_average_measurements <- lm(average_ammonium~sample_time,
                                  data=reg_data)
summary(linear_reg_average_measurements)

lab_data %>% 
  ggplot(aes(x=sample_time))+
  geom_point(aes(y=ammonium_values))+
 # geom_line(aes(y=average_ammonium,color="blue"))+
  geom_line(data = fortify(linear_reg_all_measurements), 
            aes(x = sample_time, y = .fitted),
            color="Red",
            size=1)+
  theme_malte()+
  xlab("Sample time [min]")+
  ylab("Ammnoium-N concentration [mg/L]")
