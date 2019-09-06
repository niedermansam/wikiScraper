library(wikiScraper)

data("metro_systems")
data("ny")

ny <- xml2::read_html(ny)
metro_systems <- xml2::read_html(metro_systems)

# Test wiki_table #########

test_that("wiki_table works", {
  metro_table <- metro_systems %>% wiki_table()
  expect_equal(is_tibble(metro_table), TRUE)
})

