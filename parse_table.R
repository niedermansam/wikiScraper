get_table <- function(page, table = 1, header_length = "auto", skip = "auto", col_names = NULL, rm_brackets = T, rm_parens = F){

  site_html <- wikiScraper::ws_get_page(page) %>% rvest::html_nodes("table")

  table <- as.numeric(table)
  if(is.na(table)){
    stop('argument table is not an integer')
  }

  site_html <- site_html[table]

  rows <- site_html %>% rvest::html_nodes("tr")


  # Handle Table Format ##############
  # Delete title rows (rows with 1 cell)
  format <- tibble::tibble(tag = NA, ncells = NA)

  for(r in 1:length(rows)){
    row_html <- rows[r] %>% rvest::html_nodes("th, td")

    tag <- row_html[length(row_html)] %>% rvest::html_name()

    format[r,] <- c(tag, length(row_html))
  }

  format$ncells %<>% as.numeric()
  width <- format$ncells %>% max()

  if(skip == 'auto'){
    rows <- rows[!format$ncells == 1]
    format <- format[!format$ncells == 1,]

  } else if(is.interger(skip)) {
    rows <- rows[-c(1:skip),]

  } else {
    stop("argument skip must be an integer or 'auto'")
  }

  if(header_length == "auto"){
    header_length <- table(head(format$tag, 5))[['th']]
  } else if(!is.integer(header_length)){
    stop("argument header_length must be an integer or 'auto'")
  }

  # Parse Header ##################
  header_rows <- rows[1:header_length]

  colspan_list <- list()
  rowspan_list <- list()
  header_list <- list()
  text_list <- list()
  for(r in 1:header_length){

     header_list[[r]] <- header_rows[r] %>%
       rvest::html_nodes("th, td")

     text_list[[r]] <- header_list[[r]] %>%
       rvest::html_text(trim=T)

     colspan_list[[r]] <- header_list[[r]] %>%
       rvest::html_attr("colspan", default = 1) %>%
       as.numeric()

     rowspan_list[[r]] <- header_list[[r]] %>%
       rvest::html_attr("rowspan", default = 1) %>%
       as.numeric()
   }

  header_list <- list(colspan_list,
                      rowspan_list,
                      text_list)

  header_cell <- 1
  for(c in 1:table){

    if(rowspan_list[[1]][c] > 1){
        # text_list[[i]] %<>% append("", c - 1)
        # colspan_list[[i]] %<>% append(1, c - 1)
    }

    if(colspan_list[[1]][c] > 1){
      append_x <- min(c(colspan_list[[1]][c], width-c))

    }
  }

  colspan_list[[1]]
     #rows
}


#get_table(bj, 5)
#last
#foo[[1:3]]
