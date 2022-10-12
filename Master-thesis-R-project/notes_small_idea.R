#///////////////////////////////////////////////////////////////////////////////
#Notes and small ideas
#///////////////////////////////////////////////////////////////////////////////
#The purpose of this script is to save all the code to the different notes and 
#small ideas so it can be purified later to use in the thesis



#------------------------------------------------------------------------------
#testing if the exponential filtering on the one minute data have a response in
#30 minute aggregated data. It has but minor.
p11 <- data_one_min %>% 
  select(flow_AN_m3_h) %>% 
  filter_index("2020-01-02"~"2020-01-04") %>% 
  autoplot()


p21 <- data_one_min %>% 
  select(flow_influent_m3_h) %>% 
  filter_index("2020-01-02"~"2020-01-04") %>% 
  autoplot()


p12 <- data_thirty_min %>% 
  select(flow_AN_m3_h) %>% 
  filter_index("2020-01-02"~"2020-01-04") %>% 
  autoplot()


p22 <- data_thirty_min %>% 
  select(flow_influent_m3_h) %>% 
  filter_index("2020-01-02"~"2020-01-04") %>% 
  autoplot()

gridExtra::grid.arrange(p11,p21,p12,p22)


