#' Coerce data into a convenient form for recoding
#'
#' This function is only a first step in a longer process. The goal here is to extract
#' -other- responses and display them in a convenient format that will allow for
#' later recoding using a different script. For step 2, use hss_recode.
#' @param dat The dataframe you want to remove entries from
#' @rdname hss_gen_prep
#'
#'
hss_gen_prep <- function(dat){
  #start by filtering 'other' response options
  data_match <- dat[ , grep("_oth_what", colnames(dat))]
  #remove NA + some empty cells (will be done again)
  empty_columns <- sapply(data_match, function(x) all(is.na(x) | x == ""))
  dat_2 <- data_match[, !empty_columns]
  #Generate a list of col names for looping
  list <- as.list(colnames(dat_2))
  #subset all listed values into a list of dfs
  dat_3 <- lapply(list, FUN = function(i) as.data.frame(x <- dat_2[[i]]))
  names(dat_3) = list
  #bind dfs for easy viewing
  dat_4 <- dplyr::bind_rows(dat_3, .id = "column_label")
  names(dat_4)[2] <- "Text"
  #Clean empty cells
  dat_4 <- dat_4[!dat_4$Text=="",]
  #Add reference column
  dat_4$r_name <- stringr::str_replace_all(dat_4$column_label, "_oth_what", "")
  #export file
  writexl::write_xlsx(dat_4, path = "recode.xls")

  return(dat_4)
}

































