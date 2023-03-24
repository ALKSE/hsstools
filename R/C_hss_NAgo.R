#' Convert Blank Cells to NA for Easy Splitting
#'
#' @param dat Data that needs to have blanks converted to NA
#'
#' @return Data that can be split
#' @export
#' @rdname C_hss_NAgo
C_hss_NAgo <- function(dat){
  data_na <- dat %>% select_if(is.character)
  data_na_2 <- replace(data_na, data_na=='', NA)

  q <- as.list(names(data_na_2))
  for(i in q){
    dat[[i]] <- data_na_2[[i]]
  }
  return(dat)
}
