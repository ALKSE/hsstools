#' Recode 'Other' Input
#'
#' This function seeks to finalize the reoding procedure intialized by hss_gen_prep.
#' Key to this function is the construction of an xls sheet with the modified Var values.
#' This function is currently operational, but it requires modifications for clarity.
#' @param dat actual survey data
#' @param recode the recode file which will be used to replace values
#'
#' @rdname C_hss_rewrite
#' @export
#'
C_hss_rewrite <- function(dat, recode){
  #sanity check for recode
  prep <- recode
  #split dataset for looping
  y <- split(prep, f = row.names(prep))
  #generate a stable way of indicating how the loop should operate (rows:1:n)
  z <- as.list(1:nrow(prep))
  for(i in z){
    dat[[as.character(y[[i]][[3]])]][dat[[as.character(y[[i]][[1]])]] == as.character(y[[i]][[2]])] <- as.numeric(y[[i]][[4]])}
  #This is very ugly, but there is no immediate way of doing this better (maybe purr?)
  return(dat)}
