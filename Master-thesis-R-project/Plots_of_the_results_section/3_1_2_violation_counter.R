

#Chose the working directory
setwd("C:/Users/malte/OneDrive/Dokumenter/GitHub/Master-thesis/Master-thesis-R-project")
#Load setup
violation_counter <- readxl::read_excel("violations_counter.xlsx")

violation_counter <- violation_counter %>% 
  mutate(Ammnonium=Ammnonium/2) %>% 
  mutate(Total=Total/2)

scale_constant=3

theme_malte <- function(){
  theme_light(base_size = 16)+
    theme(
      axis.line = element_line(),
      panel.border = element_blank(),
      axis.title=element_text(size=16),
      axis.text=element_text(size=12))
}

ggplot(violation_counter)+
  geom_col(aes(x=Time,
                y=Accumulated*scale_constant),
            fill="#0A9396",
           alpha=0.4)+
  geom_line(aes(x=Time,
                y=Ammnonium),
            color="Black",
            size=1)+ 
  geom_line(aes(x=Time,
                y=Total),
            color="#9B2226",
            size=1)+
  
  theme_malte()+ 
  #Create an second axis to the flow and scale it properly
  scale_y_continuous(
    name = "Hours of violation [h]",
    sec.axis = sec_axis(~.*1/scale_constant, 
                        name="Rain precipitation [mm]"))+
  theme_malte()
