#### Preamble ####
# Purpose: POL304 Week 11 Lecture Exercises
# Author: Talia Fabregas
# Date: March 25 2024
# Contact: talia.fabregas@mail.utoronto.ca
# Pre-requisites: Download regions_data.csv and animal_data.csv from 
# pol304.netlify.app/schedule and save to the week11 folder

#### Workplace setup ####
library(tidyverse)
library(mapproj)
library(ggplot2)
library(maps)
library(mapdata)
library(devtools)
library(mapcan)
library(magrittr)
library(rvest)
library(dplyr)
library(leaflet)
library(stringr)
library(janitor)

#### Map Lecture Examples ####

## UNITED STATES ##
# making basic maps
states <- map_data("state")
head(states)

# A hideous basin map
ggplot() +  geom_path(data=states, aes(x=long, y=lat, group=group),color="black", size=.5)

# A nicer map (kind of)
theme_set(theme_grey() + theme(axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank(),
                               legend.position="none"))
ggplot() +  geom_path(data=states, aes(x=long, y=lat, group=group),color="black", size=.5)+ coord_map()

# we can use geom_polygon to get a custom shade
# use polygon and path
ggplot() +  geom_polygon(data=states, aes(x=long, y=lat, group=group),color="black", size=.5)+ coord_map()

# Categorical information using hue
# Example: shade in states by region
statereg<- read.csv("week11/statereg.csv")
head(statereg)

# use left join (same idea as sql) to join latitude & longditude data with region data
states.class.map <- left_join(states, statereg, by = c("region" = "State"))
# result is a data set that contains info about coordinates and regions
head(states.class.map)

# use geom_polygon to make map again
# add an additional argument, fill=StateGroups to color code states by what
# region of the country they belong to
ggplot() +  geom_polygon(data=states.class.map, aes(x=long, y=lat, group=group, fill = StateGroups), colour = I("black"))+ coord_map()+theme(legend.position="bottom")


## CANADA ##

# Map of Canada
install_github("mccormackandrew/mapcan", build_vignettes = TRUE)
canada_map<-mapcan(boundaries = "province", type="standard",territories=TRUE)
head(canada_map)

#Set theme options:
theme_set(theme_grey() + theme(axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank(),
                               legend.position="none"))
ggplot(canada_map, aes(long, lat, group = group)) +
  geom_polygon(color="black", fill="white")

# Shade in map of canada based on 2015 election results
# get the federal elections data
# this data would be useful to improve pol478 project from last semester and make it reproducible
data("federal_election_results") 
federal_election_results %>% as.data.frame() %>% 
  dplyr::filter(election_year=="2015")->electdata
canada_ridings<-mapcan(boundaries = "ridings", type="standard",territories=TRUE)
head(canada_ridings)
# fill in the colors according to canada's political
canada_ridings %>% left_join(electdata, by="riding_code") %>%
  ggplot(aes(long, lat, group = group, fill=factor(party)))+
  geom_polygon(color="black") +scale_fill_discrete("Party", type="qual") + theme(legend.position="bottom")

# Example: GDP

# web scrape the wikipedia page to get gdp's
wikipedia_gdp <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"

gdp_html <- read_html(wikipedia_gdp)

gdp_table <- html_nodes(gdp_html, "table") %>%
  html_table(fill=TRUE)

gdp_table <-gdp_table %>% extract2(3)

library(stringr)

gdp_table <- read_html(wikipedia_gdp) %>%
  html_nodes("table") %>%
  html_table(fill=TRUE)  %>%
  extract2(3) %>%
  select(Country=1, Year=4, GDP=3) %>%
  slice(3:214) %>%
  mutate( Year=str_remove(Year, ".*\\]"),
          GDP=str_remove(GDP, ".*\\]"), GDP=parse_number(GDP), Year=parse_number(Year))

# write a csv and save it! 
write_csv(gdp_table, "week11/gdp.csv")

gdp <- read_csv("week11/gdp.csv")

# log the gdp for plotting later
gdp$log_gdp <- log(gdp$GDP)



# plot a world map without antarctica
world_map <- map_data("world") |> 
  filter(region != "Antarctica") |>
  mutate(region = ifelse(region=="USA", "United States", region))

merged_world_gdp <- left_join(world_map, gdp, by=c("region" = "Country"))

# I used light pink to represent low gdp dark pink to represent high gdp
ggplot(merged_world_gdp, aes(x=long, y=lat, group=group, fill=log_gdp)) +
  geom_polygon(color="black") +
  scale_fill_gradient(name="Log GDP", low= "mistyrose", high="#FF1493", na.value="gray") +
  labs(title="World GDP (in USD) Map")

# Example 2
animal_data <- read_csv("week11/animal.csv")
map("state", regions=c("florida", "georgia", "alabama", "mississippi", "louisiana"))

# this is the basic version of the visualization
ggplot() +
  geom_path(data=states, aes(x=long, y=lat, group=group)) +
  geom_point(data=animal_data, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(x=Longitude, y=Latitude, color=Date), shape="x", size=4.5, 
             data=animal_data) +
  xlim(c(-91, -80)) +
  ylim(c(22, 32)) + coord_map()

# Just by looking at this extremely simple visualization we see that 
# animal sightings are concentrated near the coastline

# plot color points by class of animal
ggplot() +
  geom_path(data=states, aes(x=long, y=lat, group=group)) +
  geom_point(data=animal_data, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(x=Longitude, y=Latitude, color=Species), shape="x", size=4.5, 
             data=animal_data) +
  xlim(c(-91, -80)) +
  ylim(c(22, 32)) + coord_map()

# plot color points by condition
ggplot() +
  geom_path(data=states, aes(x=long, y=lat, group=group)) +
  geom_point(data=animal_data, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(x=Longitude, y=Latitude, color=Condition), shape="x", size=4.5, 
             data=animal_data) +
  xlim(c(-91, -80)) +
  ylim(c(22, 32)) + coord_map() 
  
# do it with leaflet to show google earth map
leaflet() |> addTiles() |>
  addCircleMarkers(animal_data$Longitude, animal_data$Latitude, radius=1, animal_data$Date_)



