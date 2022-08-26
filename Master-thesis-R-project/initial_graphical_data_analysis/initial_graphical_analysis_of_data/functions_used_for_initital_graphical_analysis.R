#///////////////////////////////////////////////////////////////////////////////
#Functions to create plots for initial data investigation
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to create many standard functions which can be 
#used for a quick way to investigate various process parameters at Egå WWTP


#Effluent
plot_N_requirements <- function(df,time_period){
  
  p1 <-df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") +
    geom_line(aes(y=total_N_effluent_mg_L),
              color="blue") +
    geom_line(aes(y=flow_AN_m3_h/500),
              color="green") +
    geom_hline(yintercept=2, 
               color="orange", 
               size=.5) +
    geom_hline(yintercept=8, 
               color="red", 
               size=.5)+
    xlab("Date")+
    theme_light()+ 
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_AN_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  gridExtra::grid.arrange(p1,p2)
}




plot_N_requirements_ONE_MIN <- function(df,time_period){
  
  p1 <-df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_one_min)) +
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") #+
  #geom_line(aes(y=total_N_effluent_mg_L),
  #           color="blue") +
  # geom_line(aes(y=flow_AN_m3_h/500),
  #           color="green") +
  # geom_hline(yintercept=2, 
  #            color="orange", 
  #            size=.5) +
  # geom_hline(yintercept=8, 
  #            color="red", 
  #            size=.5)+
  # xlab("Date")+
  # theme_light()+ 
  # scale_y_continuous(
  #   name = "Ammonium/Total N concentration [mg/L]",
  #   sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
  # )
  # 
  # p2 <- df %>% 
  #   filter_index(time_period) %>% 
  #   ggplot(aes(x=time_one_min))# +
  #  # geom_line(aes(y=ammonium_load_AN_kg_h),
  #             color="black")# +
  #   xlab("Date")+ 
  #   theme_gray()+
  #   scale_y_continuous(
  #     name = "Ammonium/Total N concentration [mg/L]",
  #     sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
  #   )
  # gridExtra::grid.arrange(p1,p2)
}

plot_effluent_1 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_effluent_kg_h),
              color="black") +
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_load_effluent_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  gridExtra::grid.arrange(p1,p2)
}




#Influent
plot_influent_1 <- function(df,time_period){
  
  # p1 <- df %>% 
  #   filter_index(time_period) %>% 
  #   ggplot(aes(x=time_thirty_min)) +
  #   geom_line(aes(y=ammonium_AN_mg_L),
  #             color="black") +
  #   theme_gray()
  # 
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=rainfall_mm),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  gridExtra::grid.arrange(p3,p2)
  
}


plot_influent_2 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=rain_seven_day_accumulated),
              color="black") +
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=rain_one_day_accumulated),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=rainfall_mm),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  gridExtra::grid.arrange(p1,p2,p3)
  
}

plot_influent_3 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_AN_mg_L),
              color="black") +
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_AN_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>%
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}


#Process tank 4
plot_process_tank_4_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT4_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_PT4_SP_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_PT4_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}


plot_process_tank_4_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_load_PT4_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_PT4_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p2,p1,p3)
  
}


plot_process_tank_4_air <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT4_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=DO_PT4_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=SS_PT4_g_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}

#Process tank 3
plot_process_tank_3_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT3_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT3_SP_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_PT3_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}


plot_process_tank_3_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_load_PT3_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_PT3_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p2,p1,p3)
  
}

plot_process_tank_3_air <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT3_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=DO_PT3_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2)
  
}

#Process tank 2
plot_process_tank_2_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT2_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT2_SP_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_PT2_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}


plot_process_tank_2_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_load_PT2_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_PT2_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p2,p1,p3)
  
}

plot_process_tank_2_air <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT2_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=DO_PT2_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2)
  
}

#Process tank 1
plot_process_tank_1_N_1 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT1_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_PT1_SP_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_PT1_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}


plot_process_tank_1_N_2 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=nitrate_load_PT1_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_PT1_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=flow_AN_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p2,p1,p3)
  
}


plot_process_tank_1_air <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT1_m3_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=DO_PT1_mg_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=SS_PT1_g_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}






plot_process_tank_N_removal <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=total_N_load_PT4_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT4_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT4_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=total_N_load_PT3_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT3_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT3_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=total_N_load_PT2_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT2_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT2_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()
  
  p4 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=total_N_load_PT1_kg_h),
              color="black") +
    geom_line(aes(y=ammonium_load_PT1_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT1_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
  
}



plot_process_tank_temperature <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=T_PT1_C),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=T_PT2_C),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=T_PT3_C),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p4 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=T_PT4_C),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
  
}


plot_influent_load_month <- function(df){
  
  p2 <- df %>% 
    filter_index(month) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_load_AN_kg_h),
              color="black") +
    xlab("Date")+ 
    theme_gray()+ 
    scale_y_continuous(
      name = "Ammonium/Total N load [kg/h]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  p3 <-df %>% 
    filter_index(month) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") +
    geom_line(aes(y=total_N_effluent_mg_L),
              color="blue") +
    geom_line(aes(y=flow_AN_m3_h/500),
              color="green") +
    geom_hline(yintercept=2, 
               color="orange", 
               size=.5) +
    geom_hline(yintercept=8, 
               color="red", 
               size=.5)+
    xlab("Date")+
    theme_light()+ 
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  gridExtra::grid.arrange(p3,p2)
  
}


plot_process_tank_air_and_ss <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT4_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT4_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT4_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT3_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT3_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT3_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT2_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT2_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT2_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  p4 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT1_m3_h/50),
              color="Green") +
    geom_line(aes(y=ammonium_load_PT1_kg_h),
              color="red") +
    geom_line(aes(y=nitrate_load_PT1_kg_h),
              color="blue") +
    xlab("Date")+ 
    ylim(0,90)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate load [kg/d]",
      sec.axis = sec_axis(~.*50, name="Air flow to the tank [m3/h]")
    )
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
  
}

plot_process_tank_ss <- function(df,time_period){
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=SS_PT1_g_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=SS_PT4_g_L),
              color="black") +
    xlab("Date")+ 
    theme_gray()
  
  p3 <-df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_effluent_mg_L),
              color="black") +
    geom_line(aes(y=total_N_effluent_mg_L),
              color="blue") +
    geom_line(aes(y=flow_AN_m3_h/500),
              color="green") +
    geom_hline(yintercept=2, 
               color="orange", 
               size=.5) +
    geom_hline(yintercept=8, 
               color="red", 
               size=.5)+
    xlab("Date")+
    theme_light()+ 
    scale_y_continuous(
      name = "Ammonium/Total N concentration [mg/L]",
      sec.axis = sec_axis(~.*500, name="Flow in AN tank [m3/h]")
    )
  
  
  gridExtra::grid.arrange(p1,p2,p3)
  
}


plot_process_tank_concentration_and_air <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT4_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT4_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT4_mg_L),
              color="blue") +
    xlab("Date")+ 
    ylim(0,40)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  p2 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT3_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT3_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT3_mg_L),
              color="blue") +
    xlab("Date")+ 
    ylim(0,40)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  p3 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT2_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT2_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT2_mg_L),
              color="blue") +
    xlab("Date")+ 
    ylim(0,40)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  p4 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=airflow_PT1_m3_h/75),
              color="Green") +
    geom_line(aes(y=ammonium_PT1_mg_L),
              color="red") +
    geom_line(aes(y=nitrate_PT1_mg_L),
              color="blue") +
    xlab("Date")+ 
    ylim(0,40)+
    theme_gray()+
    scale_y_continuous(
      name = "Ammonium/nitrate concentration [mg/l]",
      sec.axis = sec_axis(~.*75, name="Air flow to the tank [m3/h]")
    )
  
  gridExtra::grid.arrange(p1,p2,p3,p4)
  
}