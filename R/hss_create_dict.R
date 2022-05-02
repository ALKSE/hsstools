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
    dict <- janitor::clean_names(readxl::read_xls(path = form, sheet = 1))
    dict[["type"]] <- gsub(".+\\s", "", dict[["type"]])
    dict <- dict[!is.na(dict[["name"]]), ]
  } else if (type == "val") {
    dict <- janitor::clean_names(readxl::read_xls(path = form, sheet = 2))
    dict <- dict[!is.na(dict[["list_name"]]), ]
  } else {
    stop(type, "is not a valid input type. Use \'var\' or \'val\'.")
  }
  return(dict)
}
