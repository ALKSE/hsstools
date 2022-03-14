#' Create a dictionary of HSS variable or value labels
#'
#' @param form Location of the XLS form
#'
#' @return A dataframe containing variable or value names, and their associated English and Arabic labels.
#' @export
#'
#' @examples
#' WIP
#' @rdname hssdict
hssdict_vars <- function(form) {
  x <- read_xls(path = form, sheet = 1)
  x[["type"]] <- gsub(".+\\s", "", x[["type"]])
  x <- x[, c("type", "name", "label::English", "label::Arabic")]
  colnames(x) <- c("type", "name", "en", "ar")
  x <- x[!is.na(x$name),]
  x
}
#' @rdname hssdict
hssdict_vals <- function(form) {
  x <- read_xls(path = form, sheet = 2)
  colnames(x) <- c("varname", "level", "en", "ar", "stata", "governorate", "district")
  x <- x[!is.na(x$varname),]
  x
}
