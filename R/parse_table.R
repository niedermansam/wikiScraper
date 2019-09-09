#' @title Scrape a table from Wikipedia page
#'
#' @description A function to parse and extract an HTML table from a Wikipedia page.
#'
#' @param page Either the url of a Wikipedia, or an object that contains a Wikipedia page
#'
#' @param n An integer that specifies which table to get data from. Defaults to 1,
#'     which retrieves the first table element in the HTML object or web page passed.
#'
#' @param skip The number of rows to skip before collecting data. This is useful for omitting
#'     full-width "title" cells. Takes an integery and defaults to 0.
#'
#' @param header_length Set to a number greater than one to deal with multi-row headers.
#'     Takes an integer and defaults to 1.
#'
#' @param col_names Optional argument that takes a character vector to name columns in
#'     the output table.
#'
#' @param exclude_brackets Whether to exclude brackets and their contents in output.
#'     Takes a boolean and defaults to TRUE.
#'
#' @param exclude_parens Whether to exclude parenthesis and their contents in output.
#'     Takes a boolean and defaults to FALSE
#'
#' @param delay Rate at which to throttle calls. There is no delay if the function is passed an HTML object
#'     (e.g. from wiki_page). Defaults to 1, can be turned off by setting
#'     to 0. Time between calls is determined by multiplying the value of this parameter with
#'     the response time by the server.
#'
#'
#' @return Returns a dataframe (tibble) that contains the data from the table
#'     specified by the argument \code{n}.
#'
#'
#' @examples
#' wiki_table("https://wikipedia.org/wiki/List_of_metro_systems")
#' wiki_table("List_of_metro_systems")
#'
#' @export wiki_table
wiki_table <- function(page,
                       n = 1,
                       header_length = "auto",
                       skip = "auto",
                       col_names = NULL,
                       rm_header_text = NULL,
                       rm_brackets = TRUE,
                       rm_parens = FALSE,
                       delay = 1){

  site_html <- wikiScraper::wiki_page(page)
  site_html %<>% rvest::html_nodes("table")

  # Get table from page ###############
  if(is.null(n)){ # If page = NULL, assume function was passed a table
    site_html <- page

  } else {
    site_html <- wikiScraper::wiki_page(page, delay = delay)
    site_html %<>% rvest::html_nodes("table")

    n <- as.numeric(n)
    if(is.na(n)){
      stop('argument n is not an integer')
    }

    site_html <- site_html[n]
  }

  format <- wiki_table_format(site_html)


  header  <- wiki_table_header(site_html,
                               header_length = header_length,
                               skip = skip,
                               col_names = col_names,
                               rm_text = rm_header_text,
                               rm_brackets = rm_brackets,
                               rm_parens = rm_parens)

  rows <- site_html %>% rvest::html_nodes("tr")

  rows <- rows[!format$rows$skip]
  format$rows <- format$rows[!format$rows$skip,]

  body_start <- as.numeric(format$header_length) + 1

  output <- data.frame(matrix(ncol = length(header), nrow = 0))
  names(output) <- header
  output %<>% tibble::as.tibble()

  body_text <- list()
  colspan_list <- list()
  rowspan_list <- list()

  for(r in body_start:length(rows)){

    current_row <- rows[r] %>%
      rvest::html_nodes("td, th")

      body_text[[r]] <- current_row %>%
      rvest::html_text(trim = T)

      if(rm_brackets){
        body_text[[r]] %<>% stringr::str_remove_all("\\[.*\\]")
      }
      if(rm_parens){
        body_text[[r]] %<>% stringr::str_remove_all("\\(.*\\)")
      }

      colspan_list[[r]] <- current_row %>%
        rvest::html_attr("colspan", 1) %>%
        as.numeric()
      rowspan_list[[r]] <- current_row %>%
        rvest::html_attr("rowspan", 1) %>%
        as.numeric()
  }

  output_index <- 1
  for(r in body_start:length(rows)){
    current_row <-  rows[r] %>%
      rvest::html_nodes("td, th")

    if(format$rows$normal[r]){
      output[output_index,] <- body_text[[r]]

    } else {


      for(c in 1:length(colspan_list[[r]])){
        if(colspan_list[[r]][c] > 1){
          append_x <- colspan_list[[r]][c] - 1
          body_text[[r]] <- body_text[[r]] %>% append(rep(body_text[[r]][c], append_x), c)

        } else if(rowspan_list[[r]][c] > 1){
          new_rows <- rowspan_list[[r]][c] - 1
          for(i in 1:new_rows){
            body_text[[r + i]] <- body_text[[r + i]] %>% append(body_text[[r]][c], c-1)
          }

        }
      }
    }

    output[output_index,] <- body_text[[r]]
    output_index <- output_index + 1
  }

  output


}

#wiki_table(bj, 5)

#' @title Parse the format of an HTML table.
#'
#' @description A function to parse an HTML table's format.
#'
#' @param table An HTML table object.
#'
#' @param skip The number of rows to skip before collecting data. This is useful for omitting
#'     full-width "title" cells. Takes an integery and defaults to 0.
#'
#' @param header_length Set to a number greater than one to deal with multi-row headers.
#'     Takes an integer and defaults to 1.
#'
#' @return Returns a list object that contains information about the format of \code{table}.
#'
#'
#' @examples
#' wiki_table("https://wikipedia.org/wiki/List_of_metro_systems")
#' wiki_table("List_of_metro_systems")
#'
#' @export wiki_table_format

wiki_table_format <- function(table, skip = "auto", header_length = "auto"){
  output <- list()

  rows <- table %>% rvest::html_nodes("tr")
  # Handle Table Format ##############
  # Delete title rows (rows with 1 cell)
  format <- tibble::tibble(tag = NA, ncells = NA, nrow_max = NA)

  for(r in 1:length(rows)){
    row_html <- rows[r] %>% rvest::html_nodes("th, td")

    nrow_max <- row_html %>%
      rvest::html_attr("rowspan", 1) %>%
      as.numeric() %>%
      max()
    # Get last tag of the row to check if it's a header or not
    # to avoid any column headers
    tag <- row_html[length(row_html)] %>% rvest::html_name()

    ncell <- length(row_html)

    tag <- ifelse(ncell == 1, "skip", tag)

    format[r,] <- c(tag, ncell, nrow_max)
  }

  format$ncells %<>% as.numeric()
  width <- format$ncells %>% max()

  format %<>% dplyr::mutate(normal = ifelse(ncells < width | nrow_max > 1, F, T))

  # Handle skip argument ##############
  if(skip == 'auto'){
    rows <- rows[!format$ncells == 1]
    format$skip <- format$ncells == 1

  } else if(is.vector(skip)) {
    rows <- rows[-skip,]
  } else {
    stop("argument skip must be an integer or 'auto'")
  }

  # Handle header_length argument ##########
  if(header_length == "auto"){
    header_length <- table(head(format$tag, 5))[['th']]
  } else if(!is.integer(header_length)){
    stop("argument header_length must be an integer or 'auto'")
  }

  output$rows <- format
  output$width <- width
  output$header_length <- header_length
  output
}



#' @title A Function to Parse an HTML Table Header
#'
#' @description Takes an HTML table, parses it's format, and returns a character
#'     vector as long as the input table is wide.
#'
#' @param table An HTML table object.
#'
#' @param skip The number of rows to skip before collecting data. This is useful for omitting
#'     full-width "title" cells. Takes a character vector of rows to skip or the string
#'      \code{"auto"}, and defaults to \code{"auto"}. If argument is set to \code{"auto"},
#'      parser will automatically remove cells that take up a full row.
#'
#' @param header_length Set to a number greater than one to deal with multi-row headers.
#'     Takes an integer and defaults to 1.
#'
#' @param exclude_brackets Whether to exclude brackets and their contents in output.
#'     Takes a boolean and defaults to TRUE.
#'
#' @param exclude_parens Whether to exclude parenthesis and their contents in output.
#'     Takes a boolean and defaults to FALSE.
#'
#'
#' @return Returns a character vector of parsed headers.
#'
#'
#' @examples
#' wiki_table("https://wikipedia.org/wiki/List_of_metro_systems")
#' wiki_table("List_of_metro_systems")
#'
#' @export wiki_table_header

wiki_table_header <- function(table,
                      header_length = "auto",
                      skip = "auto",
                      col_names = NULL,
                      rm_text = NULL,
                      rm_brackets = TRUE,
                      rm_parens = FALSE){



  rows <- table %>% rvest::html_nodes("tr")

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

  # Handle skip argument ##############
  if(skip == 'auto'){
    rows <- rows[!format$ncells == 1]
    format <- format[!format$ncells == 1,]

  } else if(is.interger(skip)) {
    rows <- rows[-c(1:skip),]
  } else {
    stop("argument skip must be an integer or 'auto'")
  }

  # Handle header_length argument ##########
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


  for(row in 1:length(text_list)){
      for(col in 1:width){


        if(rowspan_list[[row]][col] > 1){
          next_row <- row +1
          rowspan_list[[row]][col] <- 1
          for(i in next_row:length(rowspan_list)){
            text_list[[i]] %<>% append("", col - 1)
            colspan_list[[i]] %<>% append(1, col - 1)
            rowspan_list[[i]] %<>% append(1, col - 1)
          }
        }
         if(colspan_list[[row]][col] > 1){

           append_x <- min(c(colspan_list[[row]][col] - 1, width-col))

           colspan_list[[row]][col] <- 1
           colspan_list[[row]] %<>% append(rep(1,append_x), col-1)
           rowspan_list[[row]] %<>% append(rep(1,append_x), col-1)
           text_list[[row]] %<>% append(rep(text_list[[row]][col], append_x), col-1)
         }
        # print(paste("column:", col, "row:", row))
        # print("rowspan_list:")
        # print(rowspan_list)
        # print("colspan_list:")
        # print(colspan_list)
        # print("text_list")
        # print(text_list)
        # print("##########################################")
        # print("##########################################")
      }
    }

  header <- rep("",length(text_list[[1]]))

  for(i in 1:length(text_list)){
    header <- paste(header, text_list[[i]])
  }

  header %<>% stringr::str_trim()

  # Handle rm_text argument ##########
  if(!is.null(rm_text) & !is.character(rm_text)){
    stop("remove_text must be either NULL or a string")
  }

  if(is.character(rm_text)){
    header %<>%
      stringr::str_remove_all(rm_text)
  }

  # Handle  rm_brackets ###########
  if(rm_brackets){
    header %<>%
      stringr::str_remove_all('\\[.*\\]')
  }

  # Handle  rm_parens ###########
  if(rm_parens){
    header %<>% stringr::str_remove_all("\\(.*\\)")
  }

  header %<>% stringr::str_trim()

  header
     #rows
}

# bj <- wikiScraper::wiki_page("Beijing")
#bj_table <-  rvest::html_nodes(bj, "table")[5]

#bj_table %>% wiki_table_header()

# ny <- wikiScraper::wiki_page("New York City")
# ny %>%
#   wikiScraper::wiki_section("Boroughs") %>%
#   wiki_table_header(rm_parens = T)

# vmt <- wiki_page('https://www.bts.gov/statistical-products/surveys/vehicle-miles-traveled-and-vehicle-trips-state')
# vmt %>% wiki_table_header(rm_text = "Mean Census Tract estimate by urban group")
