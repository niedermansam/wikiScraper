#' @title Get HTML from Wikipedia Page
#'
#' @description Returns a list containing the contents of the requested webpage. Useful in
#'     conjunction with [wiki_table()], [wiki_card()], and [wiki_section()]. This function
#'     is a wrapper around the function [xml2::get_html()].
#'
#'
#' @param page Extension of the page, e.g. "New_York_City"
#'
#' @param url The base url of the site to visit, defaults to "https://wikipedia.org/wiki/"
#'
#' @param delay Rate at which to throttle calls. Defaults to 1, can be turned off by setting
#'     to 0. Time between calls is determined by multiplying the value of this parameter with
#'     the response time by the server.
#'
#' @return An object that contains the HTML content of the requested URL.
#'
#' @examples wiki_page("New York City")
#'# is equivelant to
#'wiki_page("https://wikipedia.org/wiki/New_York_City")
#'# and
#'#xml2::read_html("https://wikipedia.org/wiki/New_York_City")
#'
#' @export
wiki_page <-
  function(page, url = "https://wikipedia.org/wiki/", delay = 1){

    if(typeof(page) == "list"){
      return(page)
    }

    is_valid_url <- ifelse(page %>% str_detect("[:alpha:]+\\.[:alpha:]+"), T, F)

    if(is_valid_url){
      req_url <- page
    }else {
      page <- page %>% stringr::str_replace(" ", "_")
      req_url <- paste0(url,page)
    }

    t0 <- Sys.time()
      res_html <- req_url %>% xml2::read_html()
    t1 <- Sys.time()

    response_delay <- as.numeric(t1-t0)
    Sys.sleep(delay*response_delay)

    res_html
  }
