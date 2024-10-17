#' Coerce data into a convenient form for recoding
#'
#' This function is only a first step in a longer process. The goal here is to extract
#' -other- responses and display them in a convenient format that will allow for
#' later recoding using a different script. For step 2, use hss_rewrite.
#'
#' This function will generate two spreadsheets. Only one will need modification (recode_1)
#' The second spreadsheet is generated with the purpose of erasing the _oth selection (recode_2)
#'
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
  dat_4$q_type = dat_4$column_label
  #Create a secondary df
  dat_4_null <- dat_4
  dat_4_full <- dat_4
  #Add reference column
  dat_4_full$r_name <- stringr::str_replace_all(dat_4$column_label, "_oth_what", "")
  dat_4_null$r_name <- stringr::str_replace_all(dat_4$column_label, "_what", "")
  #Add value column
  dat_4_full$Value <- 1:nrow(dat_4)
  dat_4_full$Value <- replace(dat_4$Value, 1:nrow(dat_4), NA)

  dat_4_null$Value <- 1:nrow(dat_4)
  dat_4_null$Value <- replace(dat_4$Value, 1:nrow(dat_4), 0)
  #Prepare dictionary objects
  list_2 <- dict[[2]]
  list_3 <- dict[[1]]
  #Generate accurate q_type list
  list_4 <- list_3 %>% dplyr::filter(r_name %in% list)
  list_4$name <- stringr::str_replace_all(list_4$name, "_Other", "")
  list_5 <- subset(list_4, select = c(q_type, name, r_name))
  list_5$name <- stringr::str_replace_all(list_5$name, "_ar", "")
  list_6 <- list_3 %>% dplyr::filter(name %in% list_5$name)
  list_5$q_type = list_6$q_type
  #Recode q_type column in prep dataset
  dat_4_full$q_type <- dplyr::recode(dat_4_full$q_type,
                     !!!setNames(as.character(list_5$q_type), list_5$r_name))

  dat_4_null$q_type <- dplyr::recode(dat_4_null$q_type,
                                !!!setNames(as.character(list_5$q_type), list_5$r_name))

  #Relocate q_type to another position to ensure hss_rewrite works correctly
  dat_4_full <- dat_4_full %>% dplyr::relocate(q_type,
                                     .after = Value)

  dat_4_null <- dat_4_null %>% dplyr::relocate(q_type,
                                     .after = Value)

  #Subset null df to ensure only multi_questions are captured
  dat_4_null <- subset(dat_4_null, q_type == "select_multiple")


  #Run additional function to add identification key to recode_file_1
  #This value is necessary in later functions such as recode_check

  dat_4_full_key <- dat_4_full %>% mutate(key = 0)

  dat_filter <- dat %>% select(dat_4_full$column_label, key)

  #This looping function works to link un-edited text from the recode to id_keys
  num <- as.list(1:nrow(dat_4_full))
  for(i in num){
    a <- dat_4_full[i,]
    b <- dat_filter %>% select(a[[1]][1], key)
    names(a)[2] <- a[[1]][1]
    a <- a %>% inner_join(b, by = names(a[2]))
    dat_4_full_key[[6]][i] <- a[[6]][1]
    dat_4_full_updated <- dat_4_full_key
  }

  #Alternate formulation of the previous function (NOT IN USE DUE TO POTENTIAL ERRORS)
  #TEST_C_hss_recode_key <- function(prep, dat){
   # prep_up <- prep %>% mutate(key = 0)

    #num <- as.list(1:nrow(prep_up))
    #for(i in num){
     # a <- prep_up[i,]
     # b <- a %>% mutate(key = 0)
     # c <- as.data.frame(dat$key[dat[[b[[1]][1]]] == b[[2]][1]])
      #b[[6]][1] <- c[complete.cases(c),]
      #prep_up[[6]][i] <- b[[6]][1]
     # output <- prep_up
    #}
    #return(output)
 # }

  #export file
  writexl::write_xlsx(dat_4_full_updated, path = "recode_1.xlsx")
  writexl::write_xlsx(dat_4_null, path = "recode_2.xlsx")

  return(dat_4_full_updated)
}

































