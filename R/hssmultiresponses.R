#' Find response options for 'select multiple' questions
#'
#' @param variable The question for which response options should be returned. This is the "Q" number or, if lookup is set to TRUE, this is the new variable name.
#' @param dict_var The dictionary object containing the variable names and labels.
#' @param dict_val The dictionary object containing the value names and labels.
#' @param lookuplist The object containing the lookup list.
#' @param lookup Set to TRUE to use a lookup table for renamed variables.
#'
#' @return
#' @export
#'
#' @examples
#' WIP
hssmultiresponses <- function(variable, lookup = TRUE) {
  variable <- if(lookup == TRUE) {hssnamelookup(lookuplist, variable)} else {variable}
  x <- dict_var[dict_var["name"] == variable, ][[1]]
  y <- dict_val[dict_val["list_name"] == x, ][[3]]
  z <- lapply(y, function(x) paste(variable, x, sep = ""))
  hssnamelookup(lookuplist, z, reverse = TRUE)
}
