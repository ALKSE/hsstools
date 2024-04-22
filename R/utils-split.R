#' Split select_multiple survey variables
#'
#' Split select_multiple variables (1 2 7) into their individual components (1), (2), (7),
#' with additional mutations allowing for proper labeling of variables, inclusion of NAs and
#' no response variables, and prep-work for the reordering of split variables to match
#' the XLS-form standard.


split_2 <- function(data, dict){

  #(1) MATCHING AND FILTERING SELECT-MULTIPLE VARIABLE NAMES
  #(1) Output: Two lists, response-var-names, and no-response-var-names

  #*Filter select_multiple var_cols and create separate df (Baseline).
  select_multiple <- subset(dict[[1]], q_type == "select_multiple")
  match_1 <- matrix(nrow = 2, ncol = nrow(select_multiple))
  match_1 <- as.data.frame(match_1)
  colnames(match_1) <- select_multiple$r_name
  #*Filter select_multiple vars that are present in the actual df
  matches <- janitor::compare_df_cols(data, match_1, return = "mismatch")
  mismatches <- select_multiple %>% dplyr::filter(!(r_name %in% matches$column_name))
  mismatches_2 <- mismatches %>% dplyr::filter(r_name %in% colnames(data))
  #*Filter select_mutiple vars that are NOT present in the actual df (RTA excluded)
  mismatches_2.1 <- mismatches %>% dplyr::filter(r_name %in% colnames(data)) %>%
    dplyr::filter(!(type == "num"))

  #*Generate combined list of select_multiple variable names for later labeling
  #*Generate two lists for the processing of multi_vars with and without responses
  vars <- matches$column_name
  vars_0 <- mismatches_2.1$r_name
  vars_1 <- as.list(vars)
  vars_1.1 <- as.list(vars_0)
  vars_total <- c(vars, vars_0)
  vars_total <- as.list(vars_total)
  #------------------------------------------------------------------------
  #(2) MAKING DATA-FRAME LISTS OF SELECT MULTIPLE VARIABLES
  #(2) Output: Two dataframe lists, response-vars, and no-response-vars

  #*Generate a list for multi_vars with responses
  first.step <- lapply(vars_1, FUN = function(i) as.data.frame(x <- data[[i]]))
  #*Generate a list for multi_vars with NO responses
  first.step_0 <- lapply(vars_1.1, FUN = function(i) as.data.frame(x <- data[[i]]))

  #*Naming the entries in the two df lists
  names(first.step) = vars_1
  names(first.step_0) = vars_1.1
  #------------------------------------------------------------------------
  #(3) SPLITTING SELECT-MULTIPLE VARIABLES
  #(3) Output: One split-df-list containing all variables

  #*Split df list of multi_vars with responses
  second.step_1 <- lapply(first.step, FUN = function(i)
    x1 <- i %>% dplyr::mutate(ID = 1:n()) %>%
      dplyr::mutate(i = strsplit(as.character(i[[1]]), split = " ")) %>%
      unnest(i) %>%
      dplyr::mutate(Value = 1) %>%
      spread(i, Value, fill = 0) %>%
      dplyr::select(-ID))

  #*Split df list of multi_vars with NO responses
  #*Main difference in functions is the exclusion of an NA column
  #*introduced by no-response splits
  second.step_0 <- lapply(first.step_0, FUN = function(i)
    x2 <- i %>% mutate(ID = 1:n()) %>%
      dplyr::mutate(i = strsplit(as.character(i[[1]]), split = " ")) %>%
      unnest(i) %>%
      dplyr::mutate(Value = 1) %>%
      spread(i, Value, fill = 0) %>%
      dplyr::select(-ID) %>%
      dplyr::select(-'<NA>'))

  #*Removing NA from populated variables
  second.step_2 <- lapply(second.step_1, FUN = function(i)
    if('<NA>' %in% colnames(i)){x3 <- i})
  second.step_3 <- second.step_2[lengths(second.step_2) != 0]
  second.step_4 <- lapply(second.step_3, FUN = function(i)
    x4 <- i %>% dplyr::select(-'<NA>'))

  #Removing NA variables from list
  second.step_5 <- lapply(second.step_1, FUN = function(i)
    if(!('<NA>' %in% colnames(i))){x3 <- i})
  second.step_6 <- second.step_5[lengths(second.step_5) != 0]

  #*After basic processing and splitting, dataframe lists are combined
  second.step <- c(second.step_6, second.step_4, second.step_0)
  #------------------------------------------------------------------------
  #(4) CREATING A NAMING REFERENCE DF
  #(4) Output: In-between preparatory dataframe to be used as naming reference.
  z1 <- select_multiple %>% dplyr::filter(r_name %in% vars_total)
  z2 <- as.character(z1$type)
  z3 <- dict[[2]] %>% dplyr::filter(list_name %in% z2)
  z3 <- z3 %>% dplyr::rename(type = list_name)
  z3 <- z3 %>% dplyr::rename(r_name_2 = r_name)
  z4 <- z3
  z4$r_name <- z1$r_name[match(z3$type, z1$type)]

  #------------------------------------------------------------------------
  #(5) INCLUSION OF 0/NA VARIABLES
  #(5) Output: A complete (named) list of all multi_vars

  #*Seperate function that adds no response columns to df
  third.step <- function(i){
    r1 <- subset(z1, r_name == as.character(i))
    r2 <- subset(z3, type == as.character(r1$type))
    r3 <- subset(r2, select = name)
    r4 <- t(r3)
    colnames(r4) <- r3$name
    r5 <- replace(r4, 1:nrow(r2), NA)
    r6 <- as.data.frame(r5)

    matches_2 <- janitor::compare_df_cols(r6, second.step[i], return = "mismatch")
    vars_3 <- matches_2$column_name
    vars_4 <- as.list(vars_3)

    r7 <- r3 %>% dplyr::filter(!(name %in% vars_4))
    r8 <- t(r7)
    colnames(r8) <- r7$name
    r8 <- replace(r8, 1:nrow(r7), NA)
    r8 <- as.data.frame(r8)

    output <- dplyr::bind_cols(second.step[i], r8)
    return(output)}

  second.step.complete <- lapply(vars_total, third.step)
  names(second.step.complete) = vars_total
  #------------------------------------------------------------------------
  #(6) RECODING NUMERICAL COLS AND BINDING
  #(6) Output: A df with multi_vars which are split and labeled

  y2 <- vars_total

  #*Fourth step;Function that recodes colnames based on the prep
  fourth.step <- function(i){
    v <- subset(z1, r_name == as.character(i))
    v1 <- subset(z3, type == as.character(v$type))
    v2 <- second.step.complete[i]
    v3 <- as.data.frame(v2[1])
    colnames(v3) <- gsub(".*\\.", "", colnames(v3))
    colnames(v3) <- dplyr::recode(colnames(v3),
                                  !!!setNames(as.character(v1$r_name_2), v1$name))
    colnames(v3) <- paste(as.character(i), colnames(v3))
    colnames(v3) <- gsub(" ", "", colnames(v3), fixed = TRUE)
    colnames(v3) <- gsub("_all", "", colnames(v3), fixed = TRUE)
    v4 <- v3[-1]
    return(as.data.frame(v4))}

  #*Actual application of function
  output_1 <- lapply(y2, fourth.step)
  names(output_1) <- y2
  #------------------------------------------------------------------------
  #*Bind the multiple lists into one df
  output_2 <- dplyr::bind_cols(output_1)

  #*Convert select_multi NA values to zero
  output_3 <- output_2 %>% mutate_if(is.character, as.numeric)
  names_out_3 <- names(output_3)
  output_3 <- output_3 %>% mutate_at(c(names_out_3), ~replace_na(.,0))

  #*Combine split_var df with main df
  data_combined <- cbind(data, output_3)
  #------------------------------------------------------------------------
  #*Generate reference vars to be used for relocation
  var_names <- as.list(names(output_1))
  var_names_multi <<- lapply(var_names, FUN = function(i)
    v <- as.list(names(output_1[[as.character(i)]])))
  names(var_names_multi) <<- var_names
  var_output_multi <<- output_1
  #------------------------------------------------------------------------
  return(data_combined)
}
