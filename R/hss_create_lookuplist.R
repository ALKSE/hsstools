#' Create a lookup list of old & new variable names from renaming in a Stata .do file
#'
#' @param dofile Location of the Stata .do file with renaming commands
#'
#' @return A list with all old & new names of renamed variables
#' @export
#' @seealso [namelookup()] to lookup names in the generated list.
#' @examples
#' WIP
hss_create_lookuplist <- function(dofile) {
  x <- readLines(dofile)
  x <- x[startsWith(x, "rename") == TRUE]
  x <- gsub("rename\t+", "", x)
  x <- strsplit(x, "\\t+")
  return(x)
}
