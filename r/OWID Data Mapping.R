####### Corona Virus Data ########

## Data taken from from https://ourworldindata.org/covid-cases

## Notes on data
# Total and new cases + cases per million
# Total and new deaths + deaths per million
# Data on tests, including tests per thousand 
# Stringency index
# Population and GDP per capita
# Median age and population over certain ages
# Diabetes prevalence, smokers, handwashing facilities, and hospital beds per thousand people

### load librairies

library(ggmap)
library(sf)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sp)
library(mapview)
library(ggrepel)
library(ggsn)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(gifski)

### Load data ###

covid <- read_csv("owid-covid-data.csv")

### Load Map of world and modify###

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% 
  dplyr::select(19, 45) %>% 
  rename("country" = "name_long", "iso_code" = "iso_a3")

### Merge world and covid ###
world_covid <- merge(world, covid, by = "iso_code")

world_covid <- world_covid %>% 
  dplyr::select(-3, -6, -8, -10, -12:-17, -20:-29) %>% 
  filter(date >= "2020-02-01")

world_covid_geo <- st_make_valid(world_covid$geometry)

world_covid_final <- 
  world_covid %>% 
  st_drop_geometry() %>% 
  mutate(geometry = world_covid_geo) %>% 
  st_as_sf(crs = 4326)

sum(st_is_valid(world_covid_final$geometry))

## Remove countries with 0 cases and start on Feb 25
world_covid_final <-  world_covid_final %>% 
  filter(total_cases_per_million > 0)

world_covid_final <-  world_covid_final %>% 
  filter(date >= "2020-02-25")

  
## Make vector of selected colors 

map_color <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")

## Label Selected Countries

world_label_points <- st_centroid(world)

world_label_points <- cbind(world, st_coordinates(st_centroid(world)))

world_label_points <- world_label_points %>%  
  filter(country == "Australia" | country == "Brazil" | country == "Canada" | country == "China" | country == 
           "United States" | country == "Russian Federation")

### Plot test ###

world_covid_final_may20 <- world_covid_final %>% 
  filter(date == "2020-05-20")

  ggplot () +
  geom_sf(data = world) +
  geom_sf(data = world_covid_final_may20, aes(fill = total_cases_per_million)) +
  scale_fill_gradientn (colors = map_color,
                      limits = c(1, 20000),
                      breaks = c(1, 5, 20, 100, 400, 2000, 8000),
                      labels = c("1", "5", "20", "100", "400", "2,000", "8,000"),
                      trans = "log",
                      na.value = NA) +
    labs(title = "Corona Virus Cases per Million Population by Country (Feb-May 2020)",
         fill = "Total Cases per Million",
         caption = "Projection: WGS84 \nData Source: ourworldindata.org/covid-cases") +
    geom_text(data = world_label_points, aes(x = X, y = Y, label = country), size = 2.5, color = "black", fontface = "bold",
               vjust = "inward", hjust = "inward") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "aliceblue")) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"), style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.4, line_col = "black", height = unit (0.2, "cm"))

### Plot animation ###

world_animation <- ggplot () +
  geom_sf(data = world) +
  geom_sf(data = world_covid_final, aes(fill = total_cases_per_million)) +
  scale_fill_gradientn (colors = map_color,
                        limits = c(1, 20000),
                        breaks = c(1, 5, 20, 100, 400, 2000, 8000),
                        labels = c("1", "5", "20", "100", "400", "2,000", "8,000"),
                        trans = "log",
                        na.value = NA) +
  transition_states(date, 0, 3) +
  labs(title = "Corona Virus Cases per Million Population by Country (Feb-May 2020)", 
       subtitle = "{closest_state}", x = NULL, y = NULL,
       fill = "Total Cases per Million",
       caption = "Projection: WGS84 \nData Source: ourworldindata.org/covid-cases") +
  theme_void() +
  geom_text(data = world_label_points, aes(x = X, y = Y, label = country), size = 2.5, color = "black", fontface = "bold",
            vjust = "inward", hjust = "inward") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "aliceblue")) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"), style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.4, line_col = "black", height = unit (0.2, "cm"))
  

world_anim <- animate(world_animation, nframes = 86, duration = 86, height=600, width=900, renderer=gifski_renderer())

anim_save("world_anim_v2.gif", world_anim)


