#' Find 'select multiple' response variables associated with a question variable.
#'
#' @param var The variable for which 'select multiple' response variable are needed.
#' @param reverse If set to TRUE returns the new variable names. FALSE returns old names.
#'
#' @return a character string with all associated 'select multiple' variable names.
#' @export
#'
hss_get_multi <- function(var, reverse = TRUE){
  var <- hss_lookup_list(var)
  x <- hss_lookup_var(var, 2, 1)
  y <- hss_lookup_val(x, 1, 2)
  z <- lapply(y, function(x) paste(hss_lookup_list(var), x, sep = ""))
  z <- unlist(hss_lookup_list(z, reverse))
  return(z)
}
