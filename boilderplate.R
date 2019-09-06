library(wikiScraper)
library(tidyverse)

ny <- wiki_page("New_York_City")

ny_dem <- ny %>% wiki_section("Demographics")
ny_dem %>% html_nodes("table")
ny_dem %>%
  wiki_table(3, skip=1, format=c("integer", "double", "double")) %>%
  ws_tidy_names(rename =c(3,"pct_change", 2, "population"))


ny_boroughs <- ny %>% wiki_section("Boroughs")
ny_boroughs %>% wiki_table(skip=1, header_length = 2)

ny_header <- ny_boroughs %>% rvest::html_nodes("tr")
ny_header <- ny_header[2:3]

metro <- wiki_page("List_of_metro_systems")

metro_table <- metro %>%
  wiki_table(
    format = c(
      rep("string",3),
      rep('integer', 3),
      "string",
      'double')
    ) %>%
  ws_tidy_names()

metro_table$system_length %<>%
    str_replace_all("\\(.*\\)|km","") %>%
    str_trim() %>% as.double()

metro_table %>%
  ggplot(aes(y= annual_ridership_millions, x=system_length, color=yearopened, label=city)) +
  geom_point() +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans = "log10")+
  ggplot2::labs(
    title ="If You Build it, They will Ride",
    subtitle = "Metro System Length vs. Annual Ridership",
    y="Annual Ridership (Millions)",
    x="System Length (km)",
    color="Year Opened",
    caption="Data retrieved from Wikipedia using wikiScraper, and visualized with ggplot2.") +
  theme_clean()


metro_table %>%
  ggplot(aes(y= annual_ridership_millions,
             x=system_length,
             color=yearopened,
             label=city)) +
  geom_point() +
  geom_text(data = metro_table %>% filter(annual_ridership_millions > 1700),
            vjust=0,
            nudge_y=100) +
  ggplot2::labs(
    title ="If You Build it, They will Ride",
    subtitle = "Metro System Length vs. Annual Ridership",
    y="Annual Ridership (Millions)",
    x="System Length (km)",
    color="Year Opened",
    caption="Data retrieved from Wikipedia using wikiScraper, and visualized with ggplot2.") +
  theme_clean()

library(leaflet)

cali_power <- wiki_page("List of power stations in California")
cali_gas <- cali_power %>%
  wiki_table(4) %>%
  ws_tidy_names() %>%
  mutate(refs = NULL,
         lat = coords %>%
           str_extract("\\d+\\.\\d+.(N|S)") %>%
           str_remove("°(N|S)") %>%
           as.double(),
         lon =  coords %>%
           str_extract("\\d+\\.\\d+.(E|W)") %>%
           str_remove("°(E|W)") %>%
           as.double(),
         label = sprintf("<strong>Natural Gas Plant</strong><br>%s <br> %s MW/year", plant, capacity_mw) %>%
           lapply(htmltools::HTML)
         )
cali_gas$lon <- -1*cali_gas$lon

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = cali_gas,
                   radius = as.integer(cali_gas$capacity_mw) /100,
                   label = cali_gas$label,
                   color = "orange")

cali_gas_table <- cali_power %>%
  wiki_table(4) %>%
  ws_tidy_names()

cali_gas$coords

coords <- cali_gas_table$coords

parse_coordinates <- function(coords){
  lat = coords %>%
    str_extract("\\d+\\.\\d+.(N|S)")
  lon =  coords %>%
    str_extract("\\d+\\.\\d+.(E|W)")

  lat <- ifelse(str_detect(lat, "S"), lat %>% str_replace("^","-"), lat )
  lon <- ifelse(str_detect(lon, "W"), lon %>% str_replace("^","-"), lon)

  lat %<>% str_remove_all("°|[:alpha:]") %>% as.double()
  lon %<>% str_remove_all("°|[:alpha:]") %>% as.double()

  return(tibble(lat = lat, lon = lon))
}

cali_gas_table %>% bind_rows(parse_coordinates(cali_gas_table$coords))

coords %>%
  str_extract("\\d+\\.\\d+.(E|W)")

cali_gas %>% leaflet() %>% addTiles() %>% addMarkers(label=cali_gas$label)

site_html <- ny %>% wiki_section("Boroughs") %>% html_nodes('table')

bts_page <- xml2::read_html('https://www.bts.gov/statistical-products/surveys/vehicle-miles-traveled-and-vehicle-trips-state')

bts <- wiki_table(bts_page,
                       header_start = 2,
                       header_length = 2)


beijing <- wiki_page('Beijing')
beijing_table <- beijing %>%
  wiki_section('Administrative divisions') %>%
  rvest::html_nodes("table")
beijing_table <- beijing_table[1]

beijing_header <- beijing_table %>% rvest::html_nodes('tr')
beijing_header <- beijing_header[3:4]
beijing_header
#%>%
  #wiki_table(skip=2, header_length = 2)

ny_boroughs

