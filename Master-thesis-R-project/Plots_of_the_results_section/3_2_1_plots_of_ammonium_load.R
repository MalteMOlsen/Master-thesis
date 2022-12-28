
#choosing the the working directory to be here
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project/models_and_analysis/setup")

#Load set up file
source("setup_model_data.R")


theme_malte <- function(){
  theme_light(base_size = 16)+
    theme(
      axis.line = element_line(),
      panel.border = element_blank(),
      axis.title=element_text(size=12),
      axis.text=element_text(size=11),
      plot.tag = element_text(size = 14),
      plot.tag.position = c(0.95, 1))
}

# model_data <- model_data %>%
#   filter_index("2018-01-26"~"2021") %>% 
#   mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h))

p_2018 <- model_data %>% 
  select(ammonium_load_AN_kg_h,time_thirty_min) %>% 
  filter_index("2018-01-26"~"2018-12-31") %>% 
  as_tibble() %>% 
  ggplot(aes(x=time_thirty_min,
            y=ammonium_load_AN_kg_h))+
  geom_line()+
  theme_malte()+
  labs(x=element_blank(),
       y="Ammonium load in\nthe AN tank [kg/h]",
       tag = "2018")

p_2019 <- model_data %>% 
  select(ammonium_load_AN_kg_h,time_thirty_min) %>% 
  filter_index("2019") %>% 
  as_tibble() %>% 
  ggplot(aes(x=time_thirty_min,
             y=ammonium_load_AN_kg_h))+
  geom_line()+
  theme_malte()+
  labs(x=element_blank(),
       y=element_blank(),
       tag = "2019")

p_2020 <- model_data %>% 
  select(ammonium_load_AN_kg_h,time_thirty_min) %>% 
  filter_index("2020") %>% 
  as_tibble() %>% 
  ggplot(aes(x=time_thirty_min,
             y=ammonium_load_AN_kg_h))+
  geom_line()+
  theme_malte()+
  labs(x="Months",
       y="Ammonium load in\nthe AN tank [kg/h]",
       tag = "2020")

p_2021 <- model_data %>% 
  select(ammonium_load_AN_kg_h,time_thirty_min) %>% 
  filter_index("2021") %>% 
  as_tibble() %>% 
  ggplot(aes(x=time_thirty_min,
             y=ammonium_load_AN_kg_h))+
  geom_line()+
  theme_malte()+
  labs(x="Months",
       y=element_blank(),
       tag = "2021")

gridExtra::grid.arrange(p_2018,p_2019,p_2020,p_2021)
