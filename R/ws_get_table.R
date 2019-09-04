#' @title Scrape a Table from Wikipedia Page
#'
#' @description A function to extract a table from a Wikipedia page.
#'
#'
#'
#' @param page Either the url of a Wikipedia, or an object that contains a Wikipedia page
#'
#' @param table An integer that specifies which table to get data from. Defaults to 1,
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
#'     (e.g. from ws_get_page). Defaults to 1, can be turned off by setting
#'     to 0. Time between calls is determined by multiplying the value of this parameter with
#'     the response time by the server.
#'
#'
#' @return Returns a dataframe (tibble) that contains the data from the table
#'     specified by the table argument.
#'
#'
#' @examples
#' ws_get_table("https://wikipedia.org/wiki/List_of_metro_systems")
#' ws_get_table("List_of_metro_systems")
#'
#' @export ws_get_table
ws_get_table <- function(page,
                         table = 1,
                         skip = 0,
                         header_length = 1,
                         col_names = NULL,
                         exclude_brackets = TRUE,
                         exclude_parens = FALSE,
                         format = NULL,
                         delay = 1) {

    #ny = ws_scrape_page("New_York_City")
    #page = ny %>% ws_scrape_section("Demographics")
    #table = 3
    #header_start= 2
    #header_length = 2
    #title = 0
    #exclude_brackets = TRUE
    #exclude_parens = FALSE
    #pointer=1

  pointer = 1 + skip
  header_end = skip + header_length


  # Get HTML Tables #######
  site_html <- wikiScraper::ws_get_page(page, delay = delay) %>%
    rvest::html_nodes("table")

  # if(is.character(page) && stringr::str_detect(page,"\\/")) {
  #   # Read html page requested
  #   # If a forward slash is detected, "page" is passed to read_html unchanged
  #   t0 <- Sys.time()
  #   site_html <-
  #     xml2::read_html(page) %>%
  #     rvest::html_nodes("table")
  #   t1 <- Sys.time()
  #
  #   response_delay <- as.numeric(t1-t0)
  #   Sys.sleep(throttle*response_delay)
  #
  #
  # } else if(is.character(page)){
  #   # If no forward slash is detected, append the page to the end of the url for Wikipedia
  #   url <- paste0("https://wikipedia.org/wiki/",page)
  #
  #   t0 <- Sys.time()
  #   site_html <-
  #     xml2::read_html(url) %>%
  #     rvest::html_nodes("table")
  #   t1 <- Sys.time()
  #
  #   response_delay <- as.numeric(t1-t0)
  #   Sys.sleep(throttle*response_delay)
  #
  #
  # } else {
  #   # If the object passed is not a string, just extract the tables
  #     site_html <- page %>%
  #       rvest::html_nodes("table")
  #     }

  # Get specified table (defaults to first table)
  site_html <- site_html[table]


  # Get header data ########
  if(is.null(col_names)){
  header <- NA
  for(row in pointer:header_end){

    # Loop through the rows of the header
    header_row <- site_html %>%
      rvest::html_node(paste0("tr:nth-child(",row,")")) %>%
      rvest::html_nodes("th, td")

    # Get header text from current row
    header_text <- header_row %>% rvest::html_text(trim=T)

    # Get the width of each element in the row (from colspan property)
    header_widths <- header_row %>%
      rvest::html_attr('colspan') %>%
      tidyr::replace_na(1) %>%
      as.integer()

    # Get the height of each element in the row (from rowspan property)
     header_heights <- header_row %>%
       rvest::html_attr('rowspan') %>%
       tidyr::replace_na(1) %>%
       as.integer()

     # Create a temporary header to store data from this row
    temp_header <- c()
    for(col in 1:length(header_row)){
      # Loop through each column (cell) in the row

      cell_text <- header_text[col] # get text
      cell_width <- as.integer(header_widths[col]) # get width
      cell_height <- as.integer(header_heights[col]) # get height

      # Append new data from the temporary header
      temp_header <- temp_header %>% append(rep(cell_text, cell_width))

    }

    # BUG WARNING:
    # THIS NEXT STEP DOESN't WORK FOR ALL TABLES
    # REQUIRES HEADERS TO BE EVENLY DIVISIBLE BY THE LENGTH OF
    # THE SUBHEADERS COLUMN
    if(is.na(header[1])){
      # If header is NA, assign it value from temp_header
      header <- temp_header

    } else {
      # If header does exist, paste values from header onto tempheader
      header <- paste(header, temp_header)
    }

    # Increment row pointer
    pointer = pointer + 1
  }

  # Exclude brackets from header if exclude_brackets == T
  if(exclude_brackets) header <- header %>%
    stringr::str_remove_all("\\[.*\\]")

  # Exclude parenthesis from header if exclude_parens == T
  if(exclude_parens) header <- header %>%
    stringr::str_remove_all("\\(.*\\)") %>%
    stringr::str_trim()
  } else { # handle manual column names
    header=col_names

  }


  # create output dataframe #######
  final <- data.frame(matrix(ncol = length(header), nrow = 0))
  colnames(final) <- header
  final <- final %>% tibble::as_tibble()

  # get the length of the table
  table_length <- length(site_html %>% rvest::html_nodes("tr"))

  # set a variable to keep track of our index
  df_index <- 1

  # Get body data ###########
  # loop through table and append children
  for(row_index in pointer:table_length){

    # construct selector for HTML row
    node <- paste0('tr:nth-child(',row_index,') td, tr:nth-child(',row_index,') th')


    row_data <- site_html %>%
      rvest::html_node(paste0("tr:nth-child(",row_index,")")) %>%
      rvest::html_nodes("th, td")

    # get a vector of cells in that row
    row_text <- row_data %>%
      rvest::html_text(trim=T)

    col_widths <- row_data %>%
      rvest::html_attr('colspan') %>%
      tidyr::replace_na(1) %>%
      as.integer()

    if(max(as.integer(col_widths)) == length(header)){
      next;
    }

    if(max(col_widths) > 1) {
      for(col in 1:length(col_widths)){
        if(col_widths[col] > 1){
          to_append <- rep(row_text[col], col_widths[col] - 1)
          row_text <- row_text %>% append(to_append, col)
        }
      }
    }

    if(exclude_brackets) row_text <- row_text %>% stringr::str_remove_all("\\[.*\\]")
    if(exclude_parens) row_text <- row_text %>% stringr::str_remove_all("\\(.*\\)") %>% stringr::str_trim()

    # handle Wikipedia table formatting (sometimes, multiple rows share 1 cell)
    if(length(row_text) < ncol(final)){
      missing_cols = ncol(final) - length(row_data)
      prev_row <- unname(unlist(final[nrow(final),] ))
      row_text <- append(row_text, prev_row[1:missing_cols], 0)
    }

    # Append row data to ouput object and go to next row
    final[df_index,] <- row_text
    df_index = df_index + 1
  }


  # Handle formatting ######
  if(!is.null(format)){
    if(length(final) > length(format)){
      missing_formats <- length(final) - length(format)
      last_format <- format[length(format)]
      format <- format %>% append(rep(last_format, missing_formats))
    }

    for(i in 1:length(format)){

      if(format[i] == "integer" || format[i] == "int"){
        final[[i]] <- final[[i]] %>%
          stringr::str_remove_all("%|\\+|[:alpha:]|\\[.*\\]|\\(.*\\)|,| ") %>%
          as.integer()

      } else if(format[i] == 'string' || format[i] == "str" || format[i] == "character"){
        final[[i]] <- final[[i]]

      } else if(format[i] == "double" || format[i] == "num" || format[i] == "number"){
        final[[i]] <- final[[i]] %>%
          stringr::str_remove_all("%|\\+|[:alpha:]|\\[.*\\]|\\(.*\\)|,| ") %>%
          as.double()
      }
    }
  }

  return(final)
  }

