#' Find response options for 'select multiple' questions
#'
#' @param variable The question for which response options should be returned. This is the "Q" number or, if lookup is set to TRUE, this is the new variable name.
#' @param lookup Set to TRUE to use a lookup table for renamed variables.
#'
#' @return
#' @export
#'
#' @examples
#' WIP
hssmultiresponses <- function(variable, lookup = TRUE) {
  variable <- if(lookup == TRUE) {hssnamelookup(lookuplist, variable)} else {variable}

  x <- dict_var[dict_var["name"] == hssnamelookup(lookuplist, variable), ][[1]]
  y <- dict_val[dict_val["list_name"] == x, ][[2]]
  z <- lapply(y, function(x) paste(hssnamelookup(lookuplist, variable), x, sep = ""))
  unlist(hssnamelookup(lookuplist, z, reverse = TRUE))

}

#' @rdname hssmultiresponses
hsslabel_multi <- function(variable, lookup = TRUE) {
  variable <- if(lookup == TRUE) {hssnamelookup(lookuplist, variable)} else {variable}

  x <- dict_var[dict_var["name"] == variable, ][[1]]
  y <- dict_val[dict_val["list_name"] == x, ][[3]]
return(y)
}
