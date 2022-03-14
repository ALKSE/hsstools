#' Lookup old variable name by providing the new one.
#'
#' @param list The list containing the lookup value
#' @param lookup The old variable name
#' @param reverse Set to TRUE to lookup new variable name by providing the old one.
#'
#' @return A character string
#' @export
#' @seealso [hssnewnames()] to create a lookup list
#' @examples
#' WIP
hssnamelookup <- function(list, lookup, reverse = FALSE) {
  pos <- if(reverse == TRUE) {2} else {1}
  unlist(list[sapply(list, function(x) lookup %in% x)])[pos]
}
