#' Create a dictionary of HSS variable or value labels
#'
#' @param form Location of the XLS form
#' @param type Use "vars" to create a dictionary of variable names and labels. Use "vals" to create a dictionary of value names and labels.
#' @return A dataframe containing variable or value names, and their associated English and Arabic labels.
#' @export
#'
#' @examples
#' WIP
#' @rdname hssdict
hssdict <- function(form, type = "vars") {
  if (type == "vars") {
    x <- janitor::clean_names(readxl::read_xls(path = form, sheet = 1))
    x[["type"]] <- gsub(".+\\s", "", x[["type"]])
    x <- x[!is.na(x[["name"]]), ]
    x
  } else if (type == "vals") {
    x <- janitor::clean_names(readxl::read_xls(path = form, sheet = 2))
    x <- x[!is.na(x[["list_name"]]), ]
    x
  } else {
    stop(paste(type, "is not a valid input type. Use \'vars\' or \'vals\'."))
  }
}
