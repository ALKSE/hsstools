#' Convert all Select Multi NA Values to Zero
#'
#' @param dat dataframe to be used in conversion
#' @param num1 coloumn number of first NA Multi variable
#' @param num2 coloumn number of last NA Multi variable
#'
#' @return a dataframe that can be used for the relocation function
#' @export
#'
#' @rdname C_hss_NA2zero
C_hss_NA2zero <- function(dat, num1, num2){
  dat[,num1:num2] <- lapply(dat[,num1:num2], as.integer)
  names_dat <- names(dat)[num1:num2]
  dat <- dat %>% mutate_at(c(names_dat), ~replace_na(.,0))
  return(dat)
}
