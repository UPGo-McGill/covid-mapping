### Initial Mapping with Raw Data from John Hopkins University ###

### Data Source: 

# John Hopkins University Data retrieved from GitHub repo: https://github.com/CSSEGISandData/COVID-19
# Note: could be a gap between WHO data and daily reports gathered by JHU, so data is supposed to be complementary

##Notes on data
# US Data has info in terms of deaths and cases per city
# deaths_US has info on city population to normalize data
# Global cases / deaths / recovered needs to import data on world population (from UID_ISO_FIPS doc) in order to normalize the data
# Canada / China / Australia reported at the province / state level only for the global data sources


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
library(scales)

### Load Map of world and modify ###

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% 
  dplyr::select(19, 45) %>% 
  rename("country" = "name_long", "iso_code" = "iso_a3")

##### GLOBAL CASES #######

### Upload time_series_covid19_confirmed_global from data ###

# Make data tidy and turn it into an sf table
time_series_covid19_confirmed_global <- time_series_covid19_confirmed_global %>% 
  pivot_longer(cols = 5:122,
               names_to = "date",
               values_to = "cases") %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# Create a vector of brewer colors 
color_brewer1 <- c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33", "#b30000")

# Plot dot density map for world cases for May 18th 2020
time_series_covid19_confirmed_global %>% 
  filter(date == "5/18/20") %>% 
  ggplot() +
  geom_sf(data = world, fill = "grey70") +
  geom_sf(data = time_series_covid19_confirmed_global, aes(color = cases, size = cases)) + 
  scale_color_gradientn (colors = color_brewer1,
                        limits = c(1, 1508308),
                        breaks = c(1, 1000, 50000, 100000, 500000, 1500000),
                        labels = c("1", "1,000", "50,000", "100,000", "500,000", "1,500,000+"),
                        trans = "log",
                        na.value = NA)+
  scale_size_continuous(range = c(0.25,2),
                        limits = c(1, 1508308),
                        breaks = c(1, 1000, 50000, 100000, 500000, 1500000),
                        labels = c("1", "1,000", "50,000", "100,000", "500,000", "1,500,000+"),
                        trans = "log") +
  guides(color= guide_legend(), size=guide_legend()) +
  labs(title = "Number of Confirmed Global Cases of Corona Virus",
       subtitle = "May 19th, 2020",
       color = "Number of cases", 
       size = "Number of cases") +
  theme_void() + 
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(color = "black"))

##### US CASES #######

## Upload time_series_covid19_confirmed_US from data ##

# Tidy data
time_series_covid19_confirmed_US <- time_series_covid19_confirmed_US %>% 
  pivot_longer(cols = 12:129,
               names_to = "date",
               values_to = "cases") %>% 
  st_as_sf(coords = c("Long_", "Lat"), crs = 4326) %>% 
  st_transform(5070) %>% 
  select(-8, -9) %>% 
  rename(city = "Admin2")

## Upload US state file from census.gov ##
us_states <- st_read("2018_US_states", "cb_2018_us_state_500k") %>% 
  st_as_sf(crs = 4269) %>% 
  st_transform(5070)

## Create US mainland states
us_mainland <- us_states %>% 
  filter(!STUSPS %in% c("PR", "AS", "VI", "HI", "GU", "MP", "AK"))

## US cases on 5/18/20 with more than 5000 cases
us_cases_5000 <- time_series_covid19_confirmed_US %>% 
  filter(date == "5/18/20" & cases >= 5000)

## color brewer 2
color_brewer2 <-  c("#c994c7", "#df65b0", "#e7298a", "#ce1256", "#91003f")

## Labels ##
# Label states
us_mainlandstates_points <- st_centroid(us_mainland)

us_mainlandstates_points <- cbind(us_mainland, st_coordinates(st_centroid(us_mainland$geometry)))

#Label Selected Cities
us_cities_label <-  us_cases_5000 %>% 
  filter(city == "New York" | city == "Cook" | city == "Los Angeles" | city == "Philadelphia" | city == 
           "Miami-Dade" | city == "Dallas")

## Plot test

  ggplot() +
  geom_sf(data = us_mainland, fill = "palegreen") + 
  geom_sf(data = us_cases_5000, aes(color = cases, size = cases)) +
  coord_sf(xlim = st_bbox(us_mainland) [c(1,3)],
           ylim = st_bbox(us_mainland) [c(2,4)]) +
  scale_color_gradientn (colors = color_brewer2,
                        limits = c(5000, 200000),
                        breaks = c(5000, 10000, 20000, 40000, 60000, 200000),
                        labels = c("5,000", "10,000", "20,000", "40,000", "60,000", "193,230"),
                        trans = "log")+
  scale_size_continuous(limits = c(5000, 200000),
                        breaks = c(5000, 10000, 20000, 40000, 60000, 200000),
                        labels = c("5,000", "10,000", "20,000", "40,000", "60,000", "193,230"),
                        trans = "log") +
  guides(color= guide_legend(), size=guide_legend()) +
  geom_text(data = us_mainlandstates_points, aes(x = X, y = Y, label = STUSPS),
            size = 2) +
    geom_text_repel(data = us_cities_label,
                    mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                  y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                  label = city),
                    size = 3,
                    direction = "x",
                    hjust = 1,
                    point.padding = 1
                    ) +
  labs (title = "Mainland US Cities with more than 5,000 Corona Virus Cases",
        subtitle = "May 18th, 2020",
        color = "Number of cases",
        size = "Number of cases") +
  theme_void() +
  theme(legend.justification = c(0.1, 0.25),
        legend.text = element_text(size = 10),
        legend.background = element_rect(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.background = element_rect(fill = "aliceblue"))
