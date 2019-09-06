#' @title Tidy column names in tables
#'
#' @description A helper function to clean up column names.
#'
#'
#' @param data The data frame or tibble to tidy.
#' @param replace A vector of regular expressions and character string. The first element in the vector is
#'     replaced by the second, the third is replaced by the fourth, and so on. If a vector with an odd length is
#'     passed, the last element will be ignored
#'     will replace the first value with the second. In other words c("foo","bar") in the title
#'     would replace all occurances of "foo" with "bar" in the new menu.
#' @param remove A regular expression or vector of regular expressions to remove in all header names. For example,
#'     the argument c("aa","b") would delete ALL occurances of the characters "aa" and "b".
#' @param rename A vector of the form c("old_name1", "new_name1", "old_name2", "new_name2"). In this case, "old_name1"
#'     or "old_name2" could be replaced with an integer indicating the column index in which the new name should be inserted.
#' @param lowercase A boolean indicating whether the output names should be forced to lowercase. Defaults to TRUE.
#'
#'
#' @note Order of Operations: 1) replace punctuation and spaces with underscores, 2) rename,
#'     3) replace_all, 4) remove, 5) to lowercase. This could impact the results of the function.
#'
#' @examples wiki_table("List_of_metro_systems") %>% ws_tidy_names()
#'
#' @export
ws_tidy_names <- function(data, replace_all=c(""), remove=NULL, rename=NULL, lowercase=TRUE){

  output_names <- names(data);

  output_names <- output_names %>%
    stringr::str_replace_all(" |[:punct:]", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_remove_all("^_|_$")

  i=1
  while(i<=length(rename)){
    if(length(rename) - i + 1 < 2) break;
    if(!is.na(suppressWarnings(as.integer(rename[i])))){
      output_names[as.integer(rename[i])] <- rename[i+1]
    } else {
      replace_regex <- paste0("^",rename[i],"$")
      output_names %<>% stringr::str_replace(replace_regex, rename[i+1])
    }
    i = i+2
  }

  i=1
  while(i<=length(replace_all)){
    if(length(replace_all) - i + 1 < 2) break;
    output_names %<>% stringr::str_replace_all(replace_all[i], replace_all[i+1])
    i = i + 2
  }

  if(!is.null(remove)){
    remove_regex <- paste0(remove, collapse = "|")
    output_names %<>% stringr::str_remove_all(remove_regex)
  }

  if(lowercase){
    output_names %<>%
      stringr::str_to_lower()
  }
  names(data) <- output_names
  return(data)
}
