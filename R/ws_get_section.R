#' @title Get a Section from a Wikipedia Page
#'
#' @description Returns an HTML object that contains the requested section of the web page.
#'
#'
#' @param page Either a url to a wikipediea page, or an object that contains the body of a wikipedia page (e.g. from ws_scrape_page).
#' @param section The header or css id of the section to retrieve
#'
#'
#' @examples ws_get_page("List_of_metro_systems")
#'# is equivelant to
#'ws_get_page("https://en.wikipedia.org/wiki/List_of_metro_systems")
#'
#' @export
ws_get_section <-
  function(page, section){

    sectionId = paste0("#",section %>% stringr::str_replace_all(" ","_"))

    if(is.character(page)) {
      site_html <- xml2::read_html(html) %>% rvest::html_nodes("table")
    } else site_html <- page

     nodes <- site_html %>%
      rvest::html_nodes('h2, h3, p, table')

     first_index = 1
     last_index = 1
     found_id <- F
     header_type <- ""
     for(i in 1:length(nodes)){

       if(found_id && rvest::html_name(nodes[i]) == header_type){
         last_index <- i-1
         break;
       }

       if(!is.na(nodes[i] %>% rvest::html_node(sectionId))){
         found_id <- T
         first_index <- i
         header_type <- nodes[i] %>% rvest::html_name()
       }
     }
     output = nodes[first_index:last_index]
     output = paste0("<body>",paste(as.character(output),collapse=" "),"</body>") %>% xml2::read_html()
     return(output)
  }


