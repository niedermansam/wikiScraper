library(leaflet)
library(wikiScraper)
library(magrittr)
library(tidyverse)
leaflet
cali_power <- wiki_page("List of power stations in California")


cali_coal <- cali_power %>%
  wiki_table(2) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Coal Plant</strong><br>%s <br>Capacity: %s MW",
                         plant, capacity_mw) %>% lapply(htmltools::HTML)
  )

cali_nuclear <- cali_power %>%
  wiki_table(3) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Nuclear Energy</strong><br>%s <br>Capacity: %s MW",
                         name, capacity_mw) %>% lapply(htmltools::HTML)
  )

cali_gas <- cali_power %>%
  wiki_table(4) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Natural Gas Plant</strong><br>%s <br>Capacity: %s MW",
                         plant, capacity_mw) %>% lapply(htmltools::HTML)
  )



cali_hydro <- cali_power %>%
  wiki_table(5) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Hydro-Electric</strong><br>%s <br>Capacity: %s MW",
                         name, capacity_mw) %>%  lapply(htmltools::HTML)
  )

cali_pumped_hydro <- cali_power %>%
  wiki_table(6) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Pumped-Hydro</strong><br>%s <br>Capacity: %s MW",
                         name, capacity_mw) %>%  lapply(htmltools::HTML)
  )

cali_wind <- cali_power %>%
  wiki_table(7) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Wind Power</strong><br>%s <br>Capacity: %s MW",
                         name, capacity_mw) %>% lapply(htmltools::HTML)
  )

cali_solar <- cali_power %>%
  wiki_table(8) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Solar Energy</strong><br>%s <br>Capacity: %s MW",
                         station, capacity_mwac) %>% lapply(htmltools::HTML)
  )



cali_thermal_solar <- cali_power %>%
  wiki_table(9) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Thermal Solar</strong><br>%s <br>Capacity: %s MW",
                         station, capacity_mw) %>% lapply(htmltools::HTML)
  )

cali_geothermal <- cali_power %>%
  wiki_table(10) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Geothermal</strong><br>%s <br>Capacity: %s MW",
                         plant, capacity_mw) %>% lapply(htmltools::HTML)
  )
cali_biomass <- cali_power %>%
  wiki_table(11) %>%
  ws_tidy_names() %>%
  wiki_geometry() %>%
  mutate(refs = NULL,
         label = sprintf("<strong>Biomass</strong><br>%s <br>Capacity: %s MW",
                         name, capacity_mw) %>% lapply(htmltools::HTML)
  )

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = cali_gas,
                   radius = as.integer(cali_gas$capacity_mw) /100,
                   label = cali_gas$label,
                   color = "red") %>%
  addCircleMarkers(data = cali_coal,
                   radius = as.integer(cali_coal$capacity_mw) /100,
                   label = cali_coal$label,
                   color = "black") %>%
  addCircleMarkers(data = cali_nuclear,
                   radius = as.integer(cali_nuclear$capacity_mw %>% str_remove(",")) /100,
                   label = cali_nuclear$label,
                   color = "orange") %>%
  addCircleMarkers(data = cali_hydro,
                   radius = as.integer(cali_hydro$capacity_mw) /100,
                   label = cali_hydro$label,
                   color = "blue") %>%
  addCircleMarkers(data = cali_wind,
                   radius = as.integer(cali_wind$capacity_mw) /100,
                   label = cali_wind$label,
                   color = "green") %>%
  addCircleMarkers(data = cali_solar,
                   radius = as.integer(cali_solar$capacity_mwac) /100,
                   label = cali_solar$label,
                   color = "green") %>%
  addCircleMarkers(data = cali_thermal_solar,
                   radius = as.integer(cali_thermal_solar$capacity_mw) /100,
                   label = cali_thermal_solar$label,
                   color = "green") %>%
  addCircleMarkers(data = cali_geothermal,
                   radius = as.integer(cali_geothermal$capacity_mw) /100,
                   label = cali_geothermal$label,
                   color = "green") %>%
  addCircleMarkers(data = cali_biomass,
                   radius = as.integer(cali_biomass$capacity_mw) /100,
                   label = cali_biomass$label,
                   color = "green")


