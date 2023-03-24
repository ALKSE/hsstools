#' Coerce data into a convenient form for recoding
#'
#' This function is only a first step in a longer process. The goal here is to extract
#' -other- responses and display them in a convenient format that will allow for
#' later recoding using a different script. For step 2, use hss_recode.
#' @param dat The dataframe you want to remove entries from
#' @param dict Dictionary object
#' @rdname C_hss_gen_prep
#' @export
#'
C_hss_gen_prep <- function(dat, dict){
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
  dat_4 <- na.omit(dat_4)
  #Add q_type column (for later recoding)
  dat_4$q_type <- 1:nrow(dat_4)
  dat_4$q_type = dat_4$column_labe
  #Add reference column
  dat_4$r_name <- stringr::str_replace_all(dat_4$column_label, "_oth_what", "")
  #Add value column
  dat_4$Value <- 1:nrow(dat_4)
  dat_4$Value <- replace(dat_4$Value, 1:nrow(dat_4), NA)
  #Prepare dictionary objects
  list_2 <- dict[[2]]
  list_3 <- dict[[1]]
  #Generate accurate q_type list
  list_4 <- list_3 %>% dplyr::filter(r_name %in% list)
  list_4$name <- stringr::str_replace_all(list_4$name, "_Other", "")
  list_5 <- subset(list_4, select = c(q_type, name, r_name))
  list_6 <- list_3 %>% dplyr::filter(name %in% list_5$name)
  list_5$q_type = list_6$q_type
  #Recode q_type column in prep dataset
  dat_4$q_type <- dplyr::recode(dat_4$q_type,
                     !!!setNames(as.character(list_5$q_type), list_5$r_name))
  #export file
  writexl::write_xlsx(dat_4, path = "recode.xls")

  return(dat_4)
}

































