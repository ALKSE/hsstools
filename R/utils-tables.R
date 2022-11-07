# dict_var lookup function ------------------------------------------------
# Look for a value in the specified input column and return the value in the
# corresponding return column. Only takes a single input, but can return vectors
# of length >1.
.get_dict_varname <- function(var, input_col, return_col, dvar = dict_var) {
  if (!var %in% dvar[[input_col]]) {
    warning("variable name \'", var, "\' not present in column: \'", names(dvar[input_col]), "\'.")
  }

  varname <- dvar %>%
    dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(all_of(return_col)) %>%
    unlist(use.names = FALSE)

  return(varname)
}

# dict_val lookup function ------------------------------------------------
# Look for a value in the specified input column and return the value in the
# corresponding return column. Only takes a single input, but can return vectors
# of length >1.
.get_dict_valname <- function(var, input_col, return_col, dvar = dict_var, dval = dict_val) {
  if (!var %in% dval[[input_col]]) {
    warning("variable name \'", var, "\' not present in column: \'", names(dval[[input_col]]), "\'.")
  }

  valname <- dval %>%
    dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(return_col) %>%
    unlist(use.names = FALSE)

  return(valname)
}

# Get select-multiple valnames --------------------------------------------
.get_multi_valname <- function(var, df) {
  var <- .get_oldnew_varname(var)

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


# Sub-set by variable -----------------------------------------------------
# A helper function for .subset-vars. Extracts all elements in 'relevant'
# and 'r_relevant' columns. These are split into questions (The 'Q' part, later converted
# to the new names) and the answers answers/values (The '= X' part). Returns a list
# with two elements: questions and answers
.get_subset_vars <- function(var, dvar = dict_var) {
  relevant <- dvar %>%
    dplyr::filter(r_name == var) %>%
    dplyr::select(relevant, dplyr::starts_with("r_relevant"))

  questions <- stringr::str_extract(relevant, "\\$\\{.+\\}") %>%
    stringr::str_replace_all(c("\\$\\{" = "", "\\}" = "")) %>%
    na.omit()

  questions_r <- lapply(questions, function(x) .get_dict_varname(x, "name", "r_name")) %>%
    unlist()

  answers <- stringr::str_extract(relevant, ".?=.+") %>%
    stringr::str_replace_all("\\s=", "==") %>%
    stringr::str_remove_all("'") %>%
    na.omit()

  sub_vars <- list(questions = questions_r, answers = answers)

  return(sub_vars)
}

# A function to subset a dataframe based on 'relevant' columns in the XLS form.
# Checks first if the sub-setting questions exist in the dataframe (and filters nonexistent)
# and then creates input for dplyr::filter based on the sub-setting questions. Returns
# a filtered dataframe.
.subset_vars <- function(df, var) {
  var <- .get_oldnew_varname(var)

  sub_vars <- .get_subset_vars(var$new)

  questions <- sub_vars$questions[which(sub_vars$questions %in% names(df))]
  answers <- sub_vars$answers[which(sub_vars$questions %in% names(df))]

  subsetting <- paste(questions, answers, sep = " ")
  # # NB: currently this part silently fails (i.e. it does not filter) if a single
  # element of the vector [t] is invalid. The part above should make this redundant.
  df_filtered <- df %>% try(
    filter(
      !!!rlang::parse_exprs(sub_vars)
    )
  )
  return(df_filtered)
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
