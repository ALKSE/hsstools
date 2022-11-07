#' Recode 'Other' Input
#'
#' This function seeks to finalize the reoding procedure intialized by hss_gen_prep.
#' Key to this function is the construction of an xls sheet with the modified Var values.
#' This function is currently operational, but it requires modifications for clarity.
#' @param dat actual survey data
#'
#' @rdname hss_rewrite
#' @export
#'
hss_rewrite <- function(dat){
  prep <- readxl::read_xlsx("recode.xls")
  y <- split(prep, f = row.names(prep))
  z <- as.list(1:nrow(recode_prep))
  for(i in z){
    dat[[as.character(y[[i]][[3]])]][dat[[as.character(y[[i]][[1]])]] == as.character(y[[i]][[2]])] <- as.numeric(y[[i]][[4]])}
  return(dat)}
