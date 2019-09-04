#' @title Get Data from Wikipedia Card
#'
#' @description A function to extract data from the "infocards" some Wikipedia pages provide.
#'
#'
#' @param html Either a url to a wikipediea page, or an object that contains the body of a wikipedia page (e.g. from ws_scrape_page).
#' @param format Either 'long' or 'wide'. 'long' returns an output with two columns (header and data), 'wide' returns an output with a column for each data entry.
#'
#'
#' @examples ws_get_card("wiki/New_York_City")
#'
#'# OR
#'# get page THEN get card
#'
#'page <- ws_get_page("New_York_City") # get page
#'ws_get_card(page) # then get data from the card
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#'
#' @export ws_get_card

ws_get_card <-
  function(page, format="long"){

    if(is.character(page)) {
      site_html <- xml2::read_html(page) %>%
        rvest::html_nodes("table")

      }else site_html <- page

    rows <- site_html %>%
      rvest::html_nodes(".infobox") %>%
      rvest::html_nodes("tr")

    final <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(final) <- c('header', 'data')
    final <- final %>% tibble::as_tibble()

    tb_index <- 1
    lastMainHeader <- ""
    for(i in 1:length(rows)){
      header <- rows[i] %>%
        rvest::html_nodes('th') %>%
        rvest::html_text(trim=T) %>%
        stringr::str_replace('•', lastMainHeader)

      body <- rows[i] %>%
        rvest::html_nodes('td') %>%
        rvest::html_text(trim=T)

      if(length(header) == 0) next;

      if(length(body) == 0) {
        lastMainHeader <- header;
        next;
      }

      final[tb_index,] <- c(header, body)
      tb_index = tb_index + 1
    }
    final$header <- final$header %>%
      stringr::str_remove_all("\\[.*\\]")

    if(format == 'wide'){
      final <- tidyr::spread(final, header, data)
    }
    return(final)
  }


