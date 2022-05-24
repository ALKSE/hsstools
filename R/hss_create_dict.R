#' Create a dictionary of HSS variable or value labels
#'
#' Create a dictionary from an XLS form. The dictionary is stored as a dataframe.
#' This function needs to be run separately for variable- and value dictionaries.
#'
#' @param form Path to the XLS form.
#' @param type Use "var" to create a dictionary of variable names and labels. Use "val" to create a dictionary of value names and labels.
#' @return A dataframe containing variable or value names and their associated text labels.
#' @export
#'
#' @rdname hss_create_dict
hss_create_dict <- function(form, type = "var") {

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
  } else {
    stop(type, "is not a valid input type. Use \'var\' or \'val\'.")
  }
  return(dict)
}
