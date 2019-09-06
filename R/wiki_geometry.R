#' @title Get Lat/Long Coordinates from Wikipedia Page
#'
#' @description A helper function to parse Wikipedia-style geographic coordinates.
#'
#'
#' @param data A data frame that contains a column of coordinates from Wikipedia.
#' @param coordinates A character object that contains the name of the column
#'     that contains the coordiantes to parse. If missing, the function will look for
#'     columns named \code{coords} or \code{coordinates}.
#' @param delete_coords Boolean value that indicates whether to remove the original
#'     coordinates column.
#'
#' @return Returns a the dataframe passed in the argument data, with columns \code{lat}
#'     and \code{lon} appended. By default, the original \code{coordinates} column is deleted.
#'
#' @examples wiki_table("List_of_metro_systems") %>% wiki_names()
#'
#' @export
wiki_geometry <- function(data, delete_coords = T, coordinates){

  if(missing(coordinates)){
    coordinates_index <- names(data) %>% sjmisc::str_find("coords|coordinates", 1)
  } else {
    coordinates_index <- names(data) %>% sjmisc::str_find(coordinates, 0)
  }

   coords_list <- lapply(data[coordinates_index[1]],
                         function(x) stringr::str_extract(x, "\\d+\\.\\d+; -?\\d+\\.\\d+"))

   lat <- coords_list[[1]] %>%
     stringr::str_extract('\\d+\\.\\d+;') %>%
     stringr::str_replace_all(";","") %>%
     as.double()
   data$lat <- lat

   lon <- coords_list[[1]] %>%
     stringr::str_extract('; -?\\d+\\.\\d+') %>%
     stringr::str_remove_all("; ") %>%
     as.double()
   data$lon <- lon

   if(delete_coords==T) data[coordinates_index] <- NULL

   return(data)
}
