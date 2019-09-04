#' @title Get HTML from Wikipedia Page
#'
#' @description A helper function to extract a table from a Wikipedia page.
#'
#'
#' @param page Extension of the page, e.g. "New_York_City"
#' @param url The base url of the site to visit, defaults to "https://en.wikipedia.org/wiki/"
#'
#'
#' @examples ws_get_page("New_York_City")
#'# is equivelant to
#'ws_get_page("https://en.wikipedia.org/wiki/New_York_City")
#'
#' @export
ws_get_page <-
  function(page, url = "https://en.wikipedia.org/wiki/"){
    page <- page %>% stringr::str_replace(" ", "_")
    req_url <- paste0(url,page)
    res_html <- req_url %>% xml2::read_html()
    return(res_html)
  }
