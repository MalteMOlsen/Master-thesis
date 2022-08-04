
#Functions


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Data cleaning functions
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Approximate NA values in a time interval with at linear regression
NA_linear_approximate <- function(df, time_periode, column_name){
  df2 <- df %>% 
    filter_index(time_periode) %>% 
    select({{column_name}}) %>% 
    mutate(temp_column=na.approx({{column_name}})) %>% 
    select(-{{column_name}})
}

#Overwriting NA values with data from another data frame
overwrite_NA_values <- function(df_with_NA,
                                df_to_overwrite,
                                column_name){
  df <- df_with_NA %>% 
    left_join(df_to_overwrite) %>%  
    mutate(temp=if_else(is.na({{column_name}}), 
                        temp_column, 
                        {{column_name}})) %>% 
    select(-temp_column) %>% 
    select(-{{column_name}}) %>% 
    rename({{column_name}}:=temp)
}


#Finding how many hours have a NA in a day
counting_values_are_NA_in_a_day <- function(df, 
                                            column)
{
  df <- df %>% 
    select({{column}}) %>% 
    filter(is.na({{column}})) %>% 
    as_tibble() %>% 
    mutate(counter=1) %>% 
    mutate(day_time=floor_date(time_thirty_min,"day")) %>% 
    select(-time_thirty_min) %>%
    group_by(day_time) %>% 
    summarise(day_count=sum(counter))
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Scenarios with insufficient operation functions/violation investigation
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Effluent
plot_N_requirements <- function(df,time_period){
  
  df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
      geom_line(aes(y=ammonium_effluent_mg_L),
                color="black") +
      geom_line(aes(y=total_N_effluent_mg_L),
                color="blue") +
      geom_hline(yintercept=2, 
                 color="orange", 
                 size=.5) +
      geom_hline(yintercept=8, 
                 color="red", 
                 size=.5)+
    xlab("Date")+
    ylab("Ammonium/Total N concentration [mg/L]")+ 
    theme_light()
  }

#Influent
plot_influent_1 <- function(df,time_period){
  
  p1 <- df %>% 
    filter_index(time_period) %>% 
    ggplot(aes(x=time_thirty_min)) +
    geom_line(aes(y=ammonium_AN_mg_L),
              color="black") +
    theme_gray()
  
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
  gridExtra::grid.arrange(p1,p2,p3)
  
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

  gridExtra::grid.arrange(p1,p2)
  
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
    geom_line(aes(y=ammonium_PT4_mg_L),
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
  
  gridExtra::grid.arrange(p1,p2,p3)
  
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
    geom_line(aes(y=ammonium_PT3_mg_L),
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
  
  gridExtra::grid.arrange(p1,p2,p3)
  
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
    geom_line(aes(y=ammonium_PT2_mg_L),
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
  
  gridExtra::grid.arrange(p1,p2,p3)
  
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
    geom_line(aes(y=ammonium_PT1_mg_L),
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
  
  gridExtra::grid.arrange(p1,p2,p3)
  
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
