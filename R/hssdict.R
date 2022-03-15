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
    x <- readxl::read_xls(path = form, sheet = 1)
    x[["type"]] <- gsub(".+\\s", "", x[["type"]])
    x <- x[, c("type", "name", "label::English", "label::Arabic")]
    colnames(x) <- c("type", "name", "en", "ar")
    x <- x[!is.na(x[["name"]]), ]
    x
  } else if (type == "vals") {
    x <- readxl::read_xls(path = form, sheet = 2)
    colnames(x) <- c("varname", "level", "en", "ar", "stata", "governorate", "district")
    x <- x[!is.na(x[["varname"]]), ]
    x
  } else {
    stop(paste(type, "is not a valid input type. Use \'vars\' or \'vals\'."))
  }
}
