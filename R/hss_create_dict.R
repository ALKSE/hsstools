#' Create a dictionary of HSS variable or value labels
#'
#' Create a dictionary from an XLS form. The dictionary is stored as a dataframe,
#' and contains the contents of the XLS form stored in a (slightly modified) format
#' that allows it to be used as a dictionary for looking up variable & value labels,
#' This function needs to be run separately for variable- and value dictionaries.
#'
#' @param form Path to the XLS form.
#' @param type Use "var" to create a dictionary of variable names and labels.
#' Use "val" to create a dictionary of value names and labels.
#' @param location For variable dictionaries, filters to the specified location to ensure that
#' proper county/district labels are available.
#' @return A dataframe containing variable or value names and their associated text labels.
#' @export
#'
#' @rdname hss_create_dict
hss_create_dict <- function(form, type, location = NULL) {

  if (type == "var") {
    dict <- readxl::read_xls(path = form, sheet = 1) %>%
      janitor::clean_names() %>%
      tidyr::separate(col = type,
                      into = c("q_type", "type"),
                      sep = " ",
                      fill = "left"
                      )
  } else if (type == "val") {
    dict <- readxl::read_xls(path = form, sheet = 2) %>%
      janitor::clean_names()
    if(!is.null(location))
      location <- tolower(location)
    dict <- dplyr::filter(dict,
                          .data$location == .env$location | is.na(.data$location))
  } else {
    stop(type, "is not a valid input type. Use \'var\' or \'val\'.")
  }
  return(dict)
}
