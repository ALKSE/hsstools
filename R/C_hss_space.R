#' Remove space(s) from text
#'
#' This function is part of the recode pipeline and is necessary for the effective use of
#' the subsequent functions hss_gen_prep and hss_rewrite. The goal of this function is to eliminate
#' spaces at the start of a text string.
#'
#' @param dat df that needs recoding
#'
#' @return a df without spaces in the begining of text strings
#'
#' @rdname C_hss_space
#' @export
#'
C_hss_space <- function(dat){
  spaced_dat <- dat[ , grep("_oth_what", colnames(dat))]
  var_names_space <- as.list(colnames(spaced_dat))

  for(i in var_names_space){
    dat[[i]] <- stringr::str_trim(dat[[i]], "right")}
  return(dat)
}




