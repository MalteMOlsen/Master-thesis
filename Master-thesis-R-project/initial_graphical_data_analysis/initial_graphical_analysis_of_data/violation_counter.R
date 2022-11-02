#///////////////////////////////////////////////////////////////////////////////
#Exceeding the legal limits
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is the count how much time the plant have been 
#operating outside the legal limits.

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/initial_graphical_data_analysis/data_cleaning_for_initial_graphical_analysis")
#Load setup
source("setup.R")

#Setting up the different data frames
#-------------------------------------------------------------------------------
#All other columns than the effluent data is removed and the months and year 
#of the different data is found.
data_legal <- data_thirty_min %>% 
  select(nitrate_effluent_mg_L,ammonium_effluent_mg_L, rainfall_mm,flow_effluent_m3_h) %>% 
  mutate(months=month(time_thirty_min))%>% 
  mutate(year_data=year(time_thirty_min)) %>% 
  mutate(day_data=as.Date(time_thirty_min)) %>%
  as_tibble() %>% 
  select(-time_thirty_min) %>% 
  mutate(total_N=ammonium_effluent_mg_L+nitrate_effluent_mg_L)

yearly_average <- data_legal %>% 
  group_by(year_data) %>% 
  summarise(flow_year=mean(flow_effluent_m3_h, 
                           na.rm=T))

daily_average <- data_legal%>% 
  group_by(day_data) %>% 
  summarise(flow_day=mean(flow_effluent_m3_h, 
                          na.rm=T))



#Adding the drought data to the data frame
data_legal <- data_legal %>% 
  left_join(yearly_average) %>% 
  left_join(daily_average) %>% 
  mutate(total_N_vv=total_N*flow_day/flow_year)




#Creating different dataframes based on the months
data_january <- data_legal %>% 
  filter(months==1)
data_february <- data_legal %>% 
  filter(months==2)
data_march <- data_legal %>% 
  filter(months==3)
data_april <- data_legal %>% 
  filter(months==4)
data_may <- data_legal %>% 
  filter(months==5)
data_june <- data_legal %>% 
  filter(months==6)
data_july <- data_legal %>% 
  filter(months==7)
data_august <- data_legal %>% 
  filter(months==8)
data_september <- data_legal %>% 
  filter(months==9)
data_october <- data_legal %>% 
  filter(months==10)
data_november <- data_legal %>% 
  filter(months==11)
data_december <- data_legal %>% 
  filter(months==12)

#Creating different data frams based on the year
data_2018 <- data_legal %>% 
  filter(year_data==2018)
data_2019 <- data_legal %>% 
  filter(year_data==2019)
data_2020 <- data_legal %>% 
  filter(year_data==2020)
data_2021 <- data_legal %>% 
  filter(year_data==2021)
data_2022 <- data_legal %>% 
  filter(year_data==2022)

#Counting violations
#-------------------------------------------------------------------------------


#Ammonium over 2 mg/L but under 8 mg/L

#Jauary
#-----------
#Total
data_january %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_january %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_january %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_january %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_january %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_january %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  filter(year_data==2022) %>% 
  nrow()




#February
#-----------
#Total
data_february %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_february %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_february %>% 
filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_february %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_february %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_february %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2022) %>% 
  nrow()


#March
#-----------
#Total
data_march %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_march %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_march %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_march %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_march %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_march %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2022) %>% 
  nrow()


#April
#-----------
#Total
data_april %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_april %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_april %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_april %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_april %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_april %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2022) %>% 
  nrow()


#May
#-----------
#Total
data_may %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_may %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_may %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_may %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_may %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()


#June
#-----------
#Total
data_june %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_june %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_june %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_june %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_june %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()


#July
#-----------
#Total
data_july %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_july %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_july %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_july %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_july %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()



#August
#-----------
#Total
data_august %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_august %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()
#2019
data_august %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()
#2020
data_august %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()
#2021
data_august %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()


#September
#-----------
#Total
data_september %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_september %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_september %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_september %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_september %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()



#October
#-----------
#Total
data_october %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_october %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_october %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_october %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_october %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()


#November
#-----------
#Total
data_november %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_november %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_november %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_november %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_november %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()



#December
#-----------
#Total
data_december %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8) %>% 
  nrow()

#2018
data_december %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_december %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_december %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_december %>% 
  filter(ammonium_effluent_mg_L>=2) %>% 
  filter(ammonium_effluent_mg_L<8)%>% 
  filter(year_data==2021) %>% 
  nrow()




#---------
#New section
#---------






#Ammonium over 8 mg/L

#January
#-----------
#Total
data_january %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_january %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_january %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_january %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_january %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_january %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()







#February
#-----------
#Total
data_february %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_february %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_february %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_february %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_february %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_february %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()



#March
#-----------
#Total
data_march %>%
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_march %>% 
  filter(ammonium_effluent_mg_L>=8) %>%
  filter(year_data==2018) %>% 
  nrow()

#2019
data_march %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_march %>% 
  filter(ammonium_effluent_mg_L>=8) %>%
  filter(year_data==2020) %>% 
  nrow()

#2021
data_march %>% 
  filter(ammonium_effluent_mg_L>=8) %>%
  filter(year_data==2021) %>% 
  nrow()

#2022
data_march %>% 
  filter(ammonium_effluent_mg_L>=8) %>%
  filter(year_data==2022) %>% 
  nrow()



#April
#-----------
#Total
data_april %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_april %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_april %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_april %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_april %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_april %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()


#May
#-----------
#Total
data_may %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_may %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_may %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_may %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_may %>% 
  filter(ammonium_effluent_mg_L>=8) %>%  
  filter(year_data==2021) %>% 
  nrow()


#June
#-----------
#Total
data_june %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_june %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_june %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_june %>% 
  filter(ammonium_effluent_mg_L>=8) %>%  
  filter(year_data==2020) %>% 
  nrow()

#2021
data_june %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#July
#-----------
#Total
data_july %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_july %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_july %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_july %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_july %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#August
#-----------
#Total
data_august %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_august %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()
#2019
data_august %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()
#2020
data_august %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()
#2021
data_august %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#September
#-----------
#Total
data_september %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_september %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_september %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_september %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_september %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#October
#-----------
#Total
data_october %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_october %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_october %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_october %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_october %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#November
#-----------
#Total
data_november %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  nrow()

#2018
data_november %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_november %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_november %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_november %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#December
#-----------
#Total
data_december %>% 
  filter(ammonium_effluent_mg_L>=8) %>%  
  nrow()

#2018
data_december %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_december %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_december %>% 
  filter(ammonium_effluent_mg_L>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_december %>% 
  filter(ammonium_effluent_mg_L>=8) %>%  
  filter(year_data==2021) %>% 
  nrow()





#---------
#New section
#---------


#Total N over 8 mg/L

#January
#-----------
#Total
data_january %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_january %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_january %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_january %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_january %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_january %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()







#February
#-----------
#Total
data_february %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_february %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_february %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_february %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_february %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_february %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()



#March
#-----------
#Total
data_march %>%
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_march %>% 
  filter(total_N_vv>=8) %>%
  filter(year_data==2018) %>% 
  nrow()

#2019
data_march %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_march %>% 
  filter(total_N_vv>=8) %>%
  filter(year_data==2020) %>% 
  nrow()

#2021
data_march %>% 
  filter(total_N_vv>=8) %>%
  filter(year_data==2021) %>% 
  nrow()

#2022
data_march %>% 
  filter(total_N_vv>=8) %>%
  filter(year_data==2022) %>% 
  nrow()



#April
#-----------
#Total
data_april %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_april %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_april %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_april %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_april %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_april %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()


#May
#-----------
#Total
data_may %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_may %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_may %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_may %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_may %>% 
  filter(total_N_vv>=8) %>%  
  filter(year_data==2021) %>% 
  nrow()


#June
#-----------
#Total
data_june %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_june %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_june %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_june %>% 
  filter(total_N_vv>=8) %>%  
  filter(year_data==2020) %>% 
  nrow()

#2021
data_june %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#July
#-----------
#Total
data_july %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_july %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_july %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_july %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_july %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#August
#-----------
#Total
data_august %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_august %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()
#2019
data_august %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()
#2020
data_august %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()
#2021
data_august %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#September
#-----------
#Total
data_september %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_september %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_september %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_september %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_september %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#October
#-----------
#Total
data_october %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_october %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_october %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_october %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_october %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#November
#-----------
#Total
data_november %>% 
  filter(total_N_vv>=8) %>% 
  nrow()

#2018
data_november %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_november %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_november %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_november %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#December
#-----------
#Total
data_december %>% 
  filter(total_N_vv>=8) %>%  
  nrow()

#2018
data_december %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_december %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_december %>% 
  filter(total_N_vv>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_december %>% 
  filter(total_N_vv>=8) %>%  
  filter(year_data==2021) %>% 
  nrow()


#--------
#New section
#-------
rain_counter <- data_legal %>% 
  group_by(months, year_data) %>% 
  summarise(total_rain=sum(rainfall_mm))

rain_counter2 <- data_legal %>% 
  group_by(months) %>% 
  summarise(total_rain=sum(rainfall_mm))







#--------
#New section
#-------

data_lega_daily_basis <- data_legal %>% 
  mutate(load_ammonium=ammonium_effluent_mg_L*flow_effluent_m3_h*10^3) %>% 
  mutate(load_total_N=total_N*flow_effluent_m3_h*10^3) %>% 
  group_by(day_data) %>% 
  summarise(mean_load_ammonium=mean(load_ammonium, 
                                    na.rm=T),
            mean_load_total_N=mean(load_total_N,
                                   na.rm=T))


#Adding the drought data to the data frame
data_lega_daily_basis <- data_lega_daily_basis %>% 
  mutate(year_data=year(day_data)) %>%
  left_join(yearly_average) %>% 
  left_join(daily_average) %>% 
  mutate(daily_ammonium=mean_load_ammonium/(flow_day*10^3)) %>% 
  mutate(daily_total_N=mean_load_total_N/(flow_day*10^3)) %>% 
  mutate(daily_total_N_vv=daily_total_N*flow_day/flow_year)



#Creating different data frams based on the year
data_2018 <- data_lega_daily_basis %>% 
  filter(year_data==2018)
data_2019 <- data_lega_daily_basis %>% 
  filter(year_data==2019)
data_2020 <- data_lega_daily_basis %>% 
  filter(year_data==2020)
data_2021 <- data_lega_daily_basis %>% 
  filter(year_data==2021)
data_2022 <- data_lega_daily_basis %>% 
  filter(year_data==2022)



counter_2018_am_2 <- data_2018 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=2)

counter_2018_am_8 <- data_2018 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=8)

counter_2018_TN_8 <- data_2018 %>% 
  select(daily_total_N_vv) %>% 
  filter(daily_total_N_vv>=8)




counter_2019_am_2 <- data_2019 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=2)

counter_2019_am_8 <- data_2019 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=8)

counter_2019_TN_8 <- data_2019 %>% 
  select(daily_total_N_vv) %>% 
  filter(daily_total_N_vv>=8)





counter_2020_am_2 <- data_2020 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=2)

counter_2020_am_8 <- data_2020 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=8)

counter_2020_TN_8 <- data_2020 %>% 
  select(daily_total_N_vv) %>% 
  filter(daily_total_N_vv>=8)




counter_2021_am_2 <- data_2021 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=2)

counter_2021_am_8 <- data_2021 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=8)

counter_2021_TN_8 <- data_2021 %>% 
  select(daily_total_N_vv) %>% 
  filter(daily_total_N_vv>=8)





counter_2022_am_2 <- data_2022 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=2)

counter_2022_am_8 <- data_2022 %>% 
  select(daily_ammonium) %>% 
  filter(daily_ammonium>=8)

counter_2022_TN_8 <- data_2022 %>% 
  select(daily_total_N_vv) %>% 
  filter(daily_total_N_vv>=8)
