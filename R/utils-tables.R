# dict_var lookup function ------------------------------------------------
# Look for a value in the specified input column and return the value in the
# corresponding return column. Only takes a single input, but can return vectors
# of length >1.
.get_dict_varname <- function(var, input_col, return_col, dict) {
  if (!var %in% dict$var[[input_col]]) {
    warning("variable name \'", var, "\' not present in column: \'", names(dict$var[input_col]), "\'.")
  }

  varname <- dict$var %>%
    dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(all_of(return_col)) %>%
    unlist(use.names = FALSE)

  return(varname)
}

# dict_val lookup function ------------------------------------------------
# Look for a value in the specified input column and return the value in the
# corresponding return column. Only takes a single input, but can return vectors
# of length >1.
.get_dict_valname <- function(var, input_col, return_col, dict) {
  if (!var %in% dict$val[[input_col]]) {
    warning("variable name \'", var, "\' not present in column: \'", names(dict$val[[input_col]]), "\'.")
  }

  valname <- dict$val %>%
    dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(return_col) %>%
    unlist(use.names = FALSE)

  return(valname)
}

# Get select-multiple valnames --------------------------------------------
.get_multi_valname <- function(var, df) {

  answers <- names(
    dplyr::select(
      df,
      dplyr::starts_with(
        stringr::str_replace(var$new, "_all", "_")
      ) & !dplyr::ends_with(c("_what", "_all"))
    )
  )
  return(answers)
}

# Calculate N for contingency tables --------------------------------------
# Calculating N works differently for 'select-one' and 'select-multiple' tables.
# make sure to use appropriate function.
.get_nval_single <- function(df, var, group) {
  # calculate N for selected grouping.
  nval <- df %>%
    dplyr::select(!!var, !!group) %>%
    dplyr::group_by(across(!!group)) %>%
    na.omit() %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::select(-!!group) %>%
    unlist()
  nval <- c(nval, total = sum(nval))

  # create N-value labels to add to table headers. Needs empty value in first
  # position to ensure no N-value is added to question name.
  nval_labs <- paste0("\n(N = ", nval, ")") %>%
    c("", .)

  return(nval_labs)
}

.get_nval_multi <- function(df, var, group) {
  # retrieve repsonse options
  var <- .get_multi_valname(var, df)
  # calculate N for selected grouping
  nval <- df %>%
    dplyr::select(!!group, !!var) %>%
    dplyr::filter(if_all(-!!group, ~ !is.na(.x))) %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::count(.) %>%
    dplyr::ungroup() %>%
    dplyr::select(-!!group) %>%
    unlist() %>%
    c(., total = sum(.))

  # create N-value labels to add to table headers. Needs empty value in first and
  # last position to ensure no N-values are added to question name and p-value.
  nval_labs <- paste0(" \n(N = ", nval, ")") %>%
    c("", ., "")

  return(nval_labs)
}
