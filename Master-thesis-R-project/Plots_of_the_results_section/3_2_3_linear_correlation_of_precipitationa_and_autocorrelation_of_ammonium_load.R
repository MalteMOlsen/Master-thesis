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
      plot.tag.position = c(1, 1))
}

model_data <- model_data %>%
  filter_index("2018-01-26"~"2021") %>% 
  mutate(ammonium_load_AN_kg_h=na.approx(ammonium_load_AN_kg_h))


model_data <- model_data %>%
  select(ammonium_load_AN_kg_h,rainfall_mm) %>% 
  mutate(rain_lag1=lag(rainfall_mm,1))%>% 
  mutate(rain_lag2=lag(rainfall_mm,2))%>% 
  mutate(rain_lag3=lag(rainfall_mm,3))%>% 
  mutate(rain_lag4=lag(rainfall_mm,4))%>% 
  mutate(rain_lag5=lag(rainfall_mm,5))%>% 
  mutate(rain_lag6=lag(rainfall_mm,6))%>% 
  mutate(rain_lag7=lag(rainfall_mm,7))%>% 
  mutate(rain_lag8=lag(rainfall_mm,8))%>% 
  mutate(rain_lag9=lag(rainfall_mm,9))%>% 
  mutate(rain_lag10=lag(rainfall_mm,10))%>% 
  mutate(rain_lag11=lag(rainfall_mm,11))%>% 
  mutate(rain_lag12=lag(rainfall_mm,12))%>% 
  mutate(rain_lag13=lag(rainfall_mm,13))%>% 
  mutate(rain_lag14=lag(rainfall_mm,14))%>% 
  mutate(rain_lag15=lag(rainfall_mm,15))%>% 
  mutate(rain_lag16=lag(rainfall_mm,16))%>% 
  mutate(rain_lag17=lag(rainfall_mm,17))%>% 
  mutate(rain_lag18=lag(rainfall_mm,18))%>% 
  mutate(rain_lag19=lag(rainfall_mm,19))%>% 
  mutate(rain_lag20=lag(rainfall_mm,20))%>% 
  mutate(rain_lag21=lag(rainfall_mm,21))%>% 
  mutate(rain_lag22=lag(rainfall_mm,22))%>% 
  mutate(rain_lag23=lag(rainfall_mm,23))%>% 
  mutate(rain_lag24=lag(rainfall_mm,24))%>%
  mutate(rain_lag25=lag(rainfall_mm,25))%>% 
  mutate(rain_lag26=lag(rainfall_mm,26))%>% 
  mutate(rain_lag27=lag(rainfall_mm,27))%>% 
  mutate(rain_lag28=lag(rainfall_mm,28))%>% 
  mutate(rain_lag29=lag(rainfall_mm,29))%>% 
  mutate(rain_lag30=lag(rainfall_mm,30))%>% 
  mutate(rain_lag31=lag(rainfall_mm,31))%>% 
  mutate(rain_lag32=lag(rainfall_mm,32))%>% 
  mutate(rain_lag33=lag(rainfall_mm,33))%>% 
  mutate(rain_lag34=lag(rainfall_mm,34))%>% 
  mutate(rain_lag35=lag(rainfall_mm,35))%>% 
  mutate(rain_lag36=lag(rainfall_mm,36))%>% 
  mutate(rain_lag37=lag(rainfall_mm,37))%>% 
  mutate(rain_lag38=lag(rainfall_mm,38))%>% 
  mutate(rain_lag39=lag(rainfall_mm,39))%>% 
  mutate(rain_lag40=lag(rainfall_mm,40))%>% 
  mutate(rain_lag41=lag(rainfall_mm,41))%>% 
  mutate(rain_lag42=lag(rainfall_mm,42))%>% 
  mutate(rain_lag43=lag(rainfall_mm,43))%>% 
  mutate(rain_lag44=lag(rainfall_mm,44))%>% 
  mutate(rain_lag45=lag(rainfall_mm,45))%>% 
  mutate(rain_lag46=lag(rainfall_mm,46))%>% 
  mutate(rain_lag47=lag(rainfall_mm,47))%>% 
  mutate(rain_lag48=lag(rainfall_mm,48))%>%
  mutate(rain_lag49=lag(rainfall_mm,49))%>%
  mutate(rain_lag50=lag(rainfall_mm,50))%>%
  mutate(rain_lag51=lag(rainfall_mm,51))%>%
  mutate(rain_lag52=lag(rainfall_mm,52))%>%
  mutate(rain_lag53=lag(rainfall_mm,53))%>%
  mutate(rain_lag54=lag(rainfall_mm,54))%>%
  mutate(rain_lag55=lag(rainfall_mm,55))%>%
  mutate(rain_lag56=lag(rainfall_mm,56))%>%
  mutate(rain_lag57=lag(rainfall_mm,57))%>%
  mutate(rain_lag58=lag(rainfall_mm,58))%>%
  mutate(rain_lag59=lag(rainfall_mm,59))%>%
  mutate(rain_lag60=lag(rainfall_mm,60))%>%
  mutate(rain_lag61=lag(rainfall_mm,61))%>%
  mutate(rain_lag62=lag(rainfall_mm,62))%>%
  mutate(rain_lag63=lag(rainfall_mm,63))%>%
  mutate(rain_lag64=lag(rainfall_mm,64))%>%
  mutate(rain_lag65=lag(rainfall_mm,65))%>%
  mutate(rain_lag66=lag(rainfall_mm,66))%>%
  mutate(rain_lag67=lag(rainfall_mm,67))%>%
  mutate(rain_lag68=lag(rainfall_mm,68))%>%
  mutate(rain_lag69=lag(rainfall_mm,69))%>%
  mutate(rain_lag70=lag(rainfall_mm,70))%>%
  mutate(rain_lag71=lag(rainfall_mm,71))%>%
  mutate(rain_lag72=lag(rainfall_mm,72))
  


temp <- model_data %>% 
  select(ammonium_load_AN_kg_h) %>% 
  fill_gaps() %>% 
  ACF(, lag_max = 72)
temp$index <- 1:(nrow(temp))

#Ammonium load AN tank
p_ammonium_lag <- temp %>% 
  ggplot(aes(x=index,
            y=acf))+
  geom_col(width = 0.05, color="Black")+
  theme_malte()+
  ylab("Correlation coefficient\nautocorrelation of\nammonium load in the AN tank")+
  xlab(element_blank())+
  labs(tag="A")



# 
# cor_data <- model_data %>% 
#   as_tibble %>% 
#   select(-time_thirty_min) %>% 
#   na.omit()
# 
# 
# round(cor(cor_data),
#       digits = 2 # rounded to 2 decimals
# )
# ct <- round(cor(cor_data),
#             digits = 2 # rounded to 2 decimals
# )
# 
# ct2 <- ct %>% as.data.frame()
# 
# ammonium_load_AN_tank_AN <- ct2$ammonium_load_AN_kg_h %>% 
#   as.data.frame()
# ct3 <- tibble::rownames_to_column(ct2, "Names")
# 
# ammonium_load_AN_tank_AN <- ct3 %>%
#   select(ammonium_load_AN_kg_h,Names) %>% 
# as.data.frame()
# 
# 
# ammonium_load_AN_tank_AN <- ammonium_load_AN_tank_AN[-1,]
# 
# 
# ammonium_load_AN_tank_AN$index <- 0:(nrow(ammonium_load_AN_tank_AN)-1)
p_rain_lag <- ammonium_load_AN_tank_AN %>% 
  ggplot(aes(x=index,
         y=ammonium_load_AN_kg_h))+
  geom_col(width = 0.05, color="Black")+
  theme_malte()+
  ylab("Correlation coefficient\nammonium load in the AN tank\nagainst precipitation")+
  xlab("Lagged time steps (step size = 30min)")+
  labs(tag="B")
         


gridExtra::grid.arrange(p_ammonium_lag,p_rain_lag)

plot_grid(p_ammonium_lag,
          p_rain_lag,
          align = "v", 
          nrow = 2,
          ncol=1,
          rel_widths=c(1,1),
          rel_heights = c(19/40,21/40))
