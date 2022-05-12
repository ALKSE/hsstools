# dict_var lookup function ------------------------------------------------
# Look for a value in the specified input column and return the value in the
# correspoding return column. Only takes a single input, but can return vectors
# of length >1.
.get_dict_varname <- function(var, input_col, return_col) {
  if (!var %in% dict_var[[input_col]]) {
    warning("variable name \'", var, "\' not present in column: \'", names(dict_var[input_col]), "\'.")
  }

  varname <- dict_var %>%
    dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(all_of(return_col)) %>%
    unlist(use.names = FALSE)

  return(varname)
}

# dict_val lookup function ------------------------------------------------
# Look for a value in the specified input column and return the value in the
# correspoding return column. Only takes a single input, but can return vectors
# of length >1.
.get_dict_valname <- function(var, input_col, return_col) {
  if (!var %in% dict_val[[input_col]]) {
    warning("variable name \'", var, "\' not present in column: \'", names(dict_val[input_col]), "\'.")
  }

  valname <- dict_val %>%
    dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(return_col) %>%
    unlist(use.names = FALSE)

  return(valname)
}

# Get old & new varname ---------------------------------------------------
# function to remove ambiguity in variable names. Takes variable name (old or new) as input
# and returns a list containing both the old and new variable name.
.get_oldnew_varname <- function(var) {

  if (var %in% dict_var[[2]] == TRUE) {
    var_old <- var
    var_new <- .get_dict_varname(var, 2, 3)
  } else if (var %in% dict_var[[3]] == TRUE) {
    var_old <- .get_dict_varname(var, 3, 2)
    var_new <- var
  } else {
    warning(var, " not in dictionary.")
  }
  var <- list(
    old = var_old,
    new = var_new
  )
  return(var)
}



# Get select-multiple valnames --------------------------------------------
.get_multi_valname <- function(var) {
var <- .get_oldnew_varname(var)

  answers <- names(
    dplyr::select(
      df,
      starts_with(
        stringr::str_replace(var$new, "_all", "_")
      ) & !ends_with("_what")
    )
  )
  return(answers)
}


# Sub-set by variable -----------------------------------------------------

.get_dict_subvar <- function(var) {
  asdf <- hss_lookup_var(var_old, 2, 8)

  if (!is.na(sub_var)) {
    sub_q <- stringr::str_extract_all(sub_var, "Q.{1,5}(?=\\})") %>%
      unlist() %>%
      stringr::str_split(" ") %>%
      unlist()
    sub_a <- stringr::str_extract_all(sub_var, "(?<=\\')\\d{1,2}(?=\\')") %>%
      unlist() %>%
      stringr::str_split(" ") %>%
      unlist()
    df <- df %>% dplyr::filter(.[hss_lookup_list(sub_q, TRUE)] == !!as.numeric(sub_a))
  }
}

# Calculate N for 'select one' tables -------------------------------------
.get_nval_single <- function(df, var, group) {
  nval <- df %>%
    select(!!var, !!group) %>%
    group_by(across(!!group)) %>%
    na.omit() %>%
    summarise(n = n()) %>%
    select(-!!group) %>%
    unlist()
  nval <- c(nval, total = sum(nval))
  return(nval)
}

# Calculate N for 'select multiple' tables --------------------------------
.get_nval_multi <- function(df, var, group) {
  var <- .get_multi_valname(var)
  nval <-df %>% select(!!group, !!var) %>%
    filter(if_all(-!!group, ~ !is.na(.x))) %>%
    group_by(across(!!group)) %>%
    count(.) %>%
    ungroup() %>%
    select(-!!group) %>%
    unlist %>%
    c(., total = sum(.))
  return(nval)
}
