#library

library(janitor)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)
library(sf)
library(transformr)
library(gganimate)
library(viridis)
library(ggthemes)
library(gifski)
library(png)
library(DT)




#Einlesen der Daten

covid19<- read.csv("~/covid_de1.csv")
demographics <- read.csv("~/demographics_de.csv")


#Vorbereitung Covid Daten
a<-covid19 %>% 
  group_by(state,date) %>% 
  summarise(cases_inc= sum(cases),deaths_inc = sum(deaths),rec_inc =sum(recovered)) %>% 
  ungroup() %>%
  complete(state, date, fill = list(cases_inc = 0, deaths_inc =0, rec_inc = 0)) %>% 
  group_by(state) %>% 
  mutate(totalcases = cumsum(cases_inc), totaldeaths = cumsum(deaths_inc), totalrecoverd = cumsum(rec_inc))

#Vorbereitung demographic data
state_pop<-demographics %>% 
  group_by(state) %>% 
  summarise(totalpop = sum(population))

#Fusion von Covid und Demographic data
covid_state_data<-a %>% 
  left_join(state_pop, by = "state") %>% 
  mutate(cases_vs_population = totalcases/totalpop, deaths_vs_totalpop = totaldeaths/totalpop,
         cases_vs_deaths = totaldeaths/totalcases, cases_vs_rec = totalrecoverd/totalcases)


covid_state_data<- covid_state_data %>% 
  mutate(date = as.Date(date))

 # If not already installed

covid_state_data <- janitor::clean_names(covid_state_data)


#Vorbereitung Map
shape_state<- st_read("~/de_state.shp", quiet = TRUE) %>% 
  rename(state  = GEN) %>% 
  select(state, geometry) %>% 
  mutate(state = as.character(state)) %>% 
  mutate(state = str_replace_all(state, "ü", "ue")) %>% 
  mutate(state = str_replace_all(state, "ä", "ae")) %>% 
  mutate(state = str_replace_all(state, "ö", "oe")) %>% 
  mutate(state = str_replace_all(state, "ß", "ss"))


#Versuch Bayern zu retten
shapy<-sf::st_buffer(shape_state, dist = 0)



#Beispiel Map
gg<- covid_state_data %>% 
  mutate(state = as.character(state)) %>%
  left_join(shape_state, by="state") %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date == "2020-06-14	") %>% 
  ggplot(aes(fill = totalcases)) + 
  geom_sf()

gg


#Animated Map

shape_state %>% 
  right_join(covid_state_data, by ="state") %>% 
  filter(date =="2020-03-01"|date =="2020-05-01"|date =="2020-07-01"|date =="2020-09-01"|date =="2020-10-10") %>% 
  ggplot(aes(fill =totalcases))+
  geom_sf()+
  transition_time(date)+
  labs(title=":{frame_time}")


#Ohne Bayern, weil defekt
b<-shape_state %>% 
  right_join(covid_state_data, by ="state") %>% 
  filter(date > "2020-03-10", date <= "2020-10-24", state != "Bayern")


c<-b %>% ggplot(aes(fill =totalcases))+
    geom_sf()+
  transition_time(date)+
  scale_fill_viridis()+
  theme_map()+
  labs(title="{frame_time}")
c

b %>% 
  ggplot(aes(fill = cases_vs_population))+
  geom_sf()+
  scale_fill_viridis()+
  transition_time(date)+
  theme_map()+
  labs(title="{frame_time}")


e<-shape_state %>% 
  right_join(covid_state_data, by ="state") %>% 
  filter(date =="2020-10-24") %>% 
  ggplot(aes(fill = cases_vs_population)) +
  geom_sf()+
  scale_fill_viridis()+
  theme_map()

e


ff<-animate(e,fps=10,end_pause =50)


gganimate(gt)
  

ggplot(aes(fill = totalcases))+
  geom_sf()
Zahgt

animate(gt,fps = 10)



