library(wikiScraper)

bj <- wiki_page("Beijing")
bj_tables <- rvest::html_nodes(bj, "table")
bj_table <- bj_tables[5]

ny <- wiki_page("New York City")
ny_tables <- rvest::html_nodes(bj, "table")
ny_table <- ny_tables[5]

get_format(bj_table)
table = bj_table
