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
  select(nitrate_effluent_mg_L,ammonium_effluent_mg_L, rainfall_mm) %>% 
  mutate(months=month(time_thirty_min))%>% 
  mutate(year_data=year(time_thirty_min)) %>% 
  as_tibble() %>% 
  select(-time_thirty_min) %>% 
  mutate(total_N=ammonium_effluent_mg_L+nitrate_effluent_mg_L)

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
  filter(total_N>=8) %>% 
  nrow()

#2018
data_january %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_january %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_january %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_january %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_january %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()







#February
#-----------
#Total
data_february %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_february %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_february %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_february %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_february %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_february %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()



#March
#-----------
#Total
data_march %>%
  filter(total_N>=8) %>% 
  nrow()

#2018
data_march %>% 
  filter(total_N>=8) %>%
  filter(year_data==2018) %>% 
  nrow()

#2019
data_march %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_march %>% 
  filter(total_N>=8) %>%
  filter(year_data==2020) %>% 
  nrow()

#2021
data_march %>% 
  filter(total_N>=8) %>%
  filter(year_data==2021) %>% 
  nrow()

#2022
data_march %>% 
  filter(total_N>=8) %>%
  filter(year_data==2022) %>% 
  nrow()



#April
#-----------
#Total
data_april %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_april %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_april %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_april %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_april %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()

#2022
data_april %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2022) %>% 
  nrow()


#May
#-----------
#Total
data_may %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_may %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_may %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_may %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_may %>% 
  filter(total_N>=8) %>%  
  filter(year_data==2021) %>% 
  nrow()


#June
#-----------
#Total
data_june %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_june %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_june %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_june %>% 
  filter(total_N>=8) %>%  
  filter(year_data==2020) %>% 
  nrow()

#2021
data_june %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#July
#-----------
#Total
data_july %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_july %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_july %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_july %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_july %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#August
#-----------
#Total
data_august %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_august %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()
#2019
data_august %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()
#2020
data_august %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()
#2021
data_august %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#September
#-----------
#Total
data_september %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_september %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_september %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_september %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_september %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#October
#-----------
#Total
data_october %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_october %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_october %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_october %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_october %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#November
#-----------
#Total
data_november %>% 
  filter(total_N>=8) %>% 
  nrow()

#2018
data_november %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_november %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_november %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_november %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2021) %>% 
  nrow()


#December
#-----------
#Total
data_december %>% 
  filter(total_N>=8) %>%  
  nrow()

#2018
data_december %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2018) %>% 
  nrow()

#2019
data_december %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2019) %>% 
  nrow()

#2020
data_december %>% 
  filter(total_N>=8) %>% 
  filter(year_data==2020) %>% 
  nrow()

#2021
data_december %>% 
  filter(total_N>=8) %>%  
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
