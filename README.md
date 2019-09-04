# wikiScraper

The ```wikiScraper``` package makes it easy to get and transform data from Wikipedia pages. The package uses ```rvest``` and ```xml2``` to get data from web pages, and ```tidyverse``` packages for transformation.

## Getting Started

```wikiScraper``` is available via github. To install, use the ```devtools``` package.

```r
install.packages('devtools')
devtools::install_github("niedermansam/wikiScraper")
```

When the installation is complete, load the ```wikiScraper``` package and you're ready to get started. The following code creates a dataframe of all of the metro systems listed on the Wikipedia page [List of metro systems.](https://en.wikipedia.org/wiki/List_of_metro_systems)

```r
library(wikiScraper)
library(tidyverse)

metro_systems <- ws_get_table("List_of_metro_systems")
metro_systems
```

If you are planning on getting information from several parts of a page (e.g. more than one table), load the full page using ```ws_get_page```. ```ws_get_page``` automatically replaces spaces (" ") with underscores ("_"), and by default concatinates the page provided to the url "https://en.wikipedia.org/wiki/". Let's say we want to get data from the page [List of power stations in California.](https://en.wikipedia.org/wiki/List_of_power_stations_in_California)

```r
# Get page from wikipedia
cali_power <- ws_get_page("List of power stations in California")

# Get natural gas plant table, the fourth table on the page
cali_gas <- ws_get_table(table_num = 4)

# For pages with lots of tables, use ws_get_section()
cali_solar <- cali_power %>%
  ws_get_section('Solar') %>% # Get HTML data for the section titled "Solar"
  ws_get_table(1) # Get the first table in the "Solar" section
```

## Handling Geography

A lot of Wikipedia pages and tables contain geographic data. ```wikiScraper``` provides a helper function to parse Wikipedia's formatting for coordinates. ```ws_get_geography``` takes a data frame as an argument, and returns the same data frame with columns "lat" and "lon" added.

```r
# Deletes "Coordinates" column, and inserts columns for "lat" and "lon"
cali_solar %>% ws_get_geography()
```

## Limitations
Some tables with complex header structures are not accurately parsed by ```ws_get_table```.
