library(leaflet)
library(leaflet.mapboxgl)
library(RSocrata)
library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)

options(mapbox.accessToken = "pk.eyJ1IjoibWRiZWgiLCJhIjoiY2swNGJmcTRwMDNtdTNjazBhamozZnQ5ZiJ9.r_wk_xYk7peilpNqvMnzzA")

zipcode_geo <- st_read("zipcode/geo_export_a149a704-979a-4b78-9a3c-29cf2ee70dd9.shp")

suppressWarnings(st_crs(zipcode_geo) <- st_crs("+proj=longlat +datum=WGS84"))

zipcode_pop <- read_csv("zipcode population.csv")

CovidTotals <- Covid %>% filter(zip_code != "Unknown") %>%
  group_by(zip_code) %>%
  summarize(total_cases = sum(cases_weekly, na.rm = TRUE))

CovidGeo <- zipcode_geo %>%
  inner_join(CovidTotals, by = c("zip" = "zip_code")) %>%
  inner_join(zipcode_pop %>% mutate(zip = as.character(zip_code)),
             by = "zip")

pal <- colorNumeric(
  palette = "Blues",
  domain = CovidGeo$total_cases)

leaflet(CovidGeo) %>% 
  #addProviderTiles(provider = providers$Stamen.TonerBackground) %>% 
  addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
  addPolygons(fillColor = ~pal(total_cases), weight = 1, color = "darkgrey", fillOpacity = 0.8)

