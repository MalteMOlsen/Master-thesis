#///////////////////////////////////////////////////////////////////////////////
#Functions to create plots for initial data investigation
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create many standard functions which can be 
#used for a quick way to investigate various process parameters at Egå WWTP


#Effluent
#------------------------------------------------------------------------------
#Plotting the ammonium, total N and flow in one graph, and the the ammonium load
#in another plot and visualizing them together.
plot_N_requirements <- function(df,time_period){
  
  p1 <-df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") +
    geom_line(aes(y=total_N_effluent_mg_L),
              color="blue") +
    geom_line(aes(y=flow_AN_m3_h/500),
              color="green") +
    #Make two horizontal lines to display the to limits
    geom_hline(yintercept=2, 
               color="orange", 
               size=.5) +
    geom_hline(yintercept=8, 
               color="red", 
               size=.5)+
    #Name the x-axis
    xlab("Date")+ 
    #Create an second axis to the flow and scale it properly
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_load_AN_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")+ 
    #Create an second axis to shift the x-axis propontionally with the first plot
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2)
}

#This plot is to show the ammonium and nitrate load of the effluent in two
#different plots but in one visualization.
plot_effluent_1 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_load_effluent_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=nitrate_load_effluent_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2)
}

#Influent
#-------------------------------------------------------------------------------

#This plot is to show the rain and the flow of the An tank in two different 
#plots but in one visualization
plot_influent_1 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=rainfall_mm),
              color="black") +
    #Name the x-axis
    xlab("Date") 
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2)
}

#This plot is made to show the rain, one day accumulated rain and seven day 
#accumulated rain in three different plots but one visualization 
plot_influent_2 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=rain_seven_day_accumulated),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=rain_one_day_accumulated),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=rainfall_mm),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#This plot is to show the ammonium concentration and ammonium load of effluent
#and the flow of the AN tank in three seperate plots but in one visualization
plot_influent_3 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_AN_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_load_AN_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>%
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#Process tank 4
#-------------------------------------------------------------------------------
#This plot is to display the ammonium and nitrate concentration and nitrate set
#point in the process tank 3 in three different plot in one visualization
plot_process_tank_4_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_PT4_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=nitrate_PT4_SP_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=nitrate_PT4_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#This plot is to display the nitrate and ammonium load and the flow in process
#tank 4 in three different plots but in one visualization.
plot_process_tank_4_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=nitrate_load_PT4_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_load_PT4_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p2,p1,p3)
}

#This plot is to display the air flow, DO concentration and SS concentration 
#of process tank 4 in three plots but in one visualization
plot_process_tank_4_air <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=airflow_PT4_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=DO_PT4_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=SS_PT4_g_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#Process tank 3
#-------------------------------------------------------------------------------
#This plot is to display the ammonium and nitrate concentration and ammonium set
#point in the process tank 3 in three different plot in one visualization
plot_process_tank_3_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_PT3_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_PT3_SP_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=nitrate_PT3_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#This plot is to display the nitrate and ammonium load and the flow in process
#tank 3 in three different plots but in one visualization.
plot_process_tank_3_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=nitrate_load_PT3_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>%
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=ammonium_load_PT3_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time serie to display at the y-axis
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p2,p1,p3)
}

#This plot is to display the air flow and DO concentration of process tank 3
#in two plots but in one visualization
plot_process_tank_3_air <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=airflow_PT3_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>%
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=DO_PT3_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2)
}

#Process tank 2
#------------------------------------------------------------------------------
#This plot is to display the ammonium and nitrate concentration and ammonium set
#point in the process tank 2 in three different plot in one visualization
plot_process_tank_2_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>%
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_PT2_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>%
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_PT2_SP_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=nitrate_PT2_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#This plot is to display the nitrate and ammonium load and the flow in process
#tank 2 in three different plots but in one visualization.
plot_process_tank_2_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=nitrate_load_PT2_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_load_PT2_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p2,p1,p3)
}

#This plot is to display the air flow and DO concentration of process tank 2 
#in two plots but in one visualization
plot_process_tank_2_air <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=airflow_PT2_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=DO_PT2_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2)
}

#Process tank 1
#-------------------------------------------------------------------------------
#This plot is to display the ammonium and nitrate concentration and ammonium set
#point in the process tank 1 in three different plot in one visualization
plot_process_tank_1_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_PT1_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_PT1_SP_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=nitrate_PT1_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#This plot is to display the nitrate and ammonium load and the flow in process
#tank 1 in three different plots but in one visualization.
plot_process_tank_1_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=nitrate_load_PT1_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_load_PT1_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p2,p1,p3)
}

#This plot is to display the air flow, DO concentration and SS concentration 
#of process tank 1 in three plots but in one visualization
plot_process_tank_1_air <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=airflow_PT1_m3_h),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=DO_PT1_mg_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=SS_PT1_g_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#Plant overview
#-------------------------------------------------------------------------------

#This plot is made to display the ammonium, nitrate and total N load 
#in all the four process tanks in four different plot, but in one visualization
plot_process_tank_N_removal <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=total_N_load_PT4_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT4_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT4_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixating  the y axis limits to be 0 and 90
    ylim(0,90)
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=total_N_load_PT3_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT3_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT3_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixating  the y axis limits to be 0 and 90
    ylim(0,90)
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=total_N_load_PT2_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT2_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT2_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixating  the y axis limits to be 0 and 90
    ylim(0,90)
  
  p4 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=total_N_load_PT1_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT1_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT1_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixating  the y axis limits to be 0 and 90
    ylim(0,90)
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3,p4)
}


#This plot is to plot the temperature profile in all the four process tanks
#in four different plots but in one visualization.
plot_process_tank_temperature <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=T_PT1_C),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=T_PT2_C),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=T_PT3_C),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p4 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=T_PT4_C),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3,p4)
}

#This plot is to plot the ammonium load to the AN tank and the nitrate and 
#ammonium concentration and flow in two plot but in one visualization.
plot_influent_load_month <- function(df){
  
  p1 <-df %>% 
    #Select time period
    filter_index(month) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") +
    geom_line(aes(y=total_N_effluent_mg_L),
              color="blue") +
    geom_line(aes(y=flow_AN_m3_h/500),
              color="green") +
    #Create the horizontal lines to visualize the limits
    geom_hline(yintercept=2, 
               color="orange", 
               size=.5) +
    geom_hline(yintercept=8, 
               color="red", 
               size=.5)+
    #Name the x-axis
    xlab("Date")+
    #Create a second y-axis to display the flow to the AN tank
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )  
  
  p2 <- df %>% 
    #Select time period
    filter_index(month) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=ammonium_load_AN_kg_h),
              color="black") +
    #Name the x-axis
    xlab("Date")+ 
    #Create a second y-axis to display the flow to the AN tank
    scale_y_continuous(
      name = "Ammonium/Total N load [kg/h]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2)
}

#This plot is made to display the ammonium and nitrate load and air flow 
#in all the four process tanks in four different plot, but in one visualization
plot_process_tank_air_and_ss <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT4_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT4_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT4_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixation the y-axis between 0 and 90
    ylim(0,90)+
    #Create a second y-axis to plot the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT3_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT3_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT3_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixation the y-axis between 0 and 90
    ylim(0,90)+
    #Create a second y-axis to plot the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT2_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT2_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT2_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Fixation the y-axis between 0 and 90
    ylim(0,90)+
    #Create a second y-axis to plot the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  p4 <- df %>% 
    #Select time period
    filter_index(time_period) %>%
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Select the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT1_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT1_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT1_kg_h),
              color="blue") +
    #Name the x-axis
    xlab("Date")+
    #Fixation the y-axis between 0 and 90
    ylim(0,90)+
    #Create a second y-axis to plot the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3,p4)
}

#This plot is to visualize the SS concentration in tank 1 and 4 and flow, 
#ammonium and nitrate concentration in three plot in one grid visualization
plot_process_tank_ss <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>%
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=SS_PT1_g_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the time series to display at the y-axis
    geom_line(aes(y=SS_PT4_g_L),
              color="black") +
    #Name the x-axis
    xlab("Date")
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") +
    geom_line(aes(y=total_N_effluent_mg_L),
              color="blue") +
    geom_line(aes(y=flow_AN_m3_h/500),
              color="green") +
    #Create the horizontal lines to visualize the limits
    geom_hline(yintercept=2, 
               color="orange", 
               size=.5) +
    geom_hline(yintercept=8, 
               color="red", 
               size=.5)+
    #Name the x-axis
    xlab("Date")+
    #Create a second axis to the air flow
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3)
}

#This plot is made to display the ammonium and nitrate concentration and air flow 
#in all the four process tanks in four different plot, but in one visualization
plot_process_tank_concentration_and_air <- function(df,time_period){
  
  p1 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT4_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT4_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT4_mg_L),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Define the limits of the y-axis
    ylim(0,40)+
    #Chose the gray theme
    theme_gray()+
    #Create a second axis to the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  p2 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT3_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT3_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT3_mg_L),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Define the limits of the y-axis
    ylim(0,40)+
    #Chose the gray theme
    theme_gray()+
    #Create a second axis to the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  p3 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT2_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT2_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT2_mg_L),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Define the limits of the y-axis
    ylim(0,40)+
    #Chose the gray theme
    theme_gray()+
    #Create a second axis to the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  p4 <- df %>% 
    #Select time period
    filter_index(time_period) %>% 
    #Select time as the x-axis
    ggplot(aes(x=time_thirty_min)) +
    #Chose the three time series to display at the y-axis
    geom_line(aes(y=airflow_PT1_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT1_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT1_mg_L),
              color="blue") +
    #Name the x-axis
    xlab("Date")+ 
    #Define the limits of the y-axis
    ylim(0,40)+
    #Chose the gray theme
    theme_gray()+
    #Create a second axis to the air flow
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  #Arrange the different plots into a grid formation, to display them all at once
  gridExtra::grid.arrange(p1,p2,p3,p4)
}