#' Create a dictionary of HSS variable or value labels
#'
#' @param form Location of the XLS form
#' @param type Use "var" to create a dictionary of variable names and labels. Use "val" to create a dictionary of value names and labels.
#' @return A dataframe containing variable or value names and their associated text labels.
#' @export
#'
#' @examples
#' WIP
#' @rdname hss_create_dict
hss_create_dict <- function(form, type = "var") {
  if (type == "var") {
    x <- janitor::clean_names(readxl::read_xls(path = form, sheet = 1))
    x[["type"]] <- gsub(".+\\s", "", x[["type"]])
    x <- x[!is.na(x[["name"]]), ]
    x
  } else if (type == "val") {
    x <- janitor::clean_names(readxl::read_xls(path = form, sheet = 2))
    x <- x[!is.na(x[["list_name"]]), ]
    x
  } else {
    stop(paste(type, "is not a valid input type. Use \'var\' or \'val\'."))
  }
}
