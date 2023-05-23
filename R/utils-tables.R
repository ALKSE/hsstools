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
        stringr::str_replace(var, "_all", "_")
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

#This function is now outdated and has been replaced by nval_multi_calc
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

#New function for calculating N for "select multiple" questions
nval_multi_calc <- function(base_table){
  z <- as.list(1:ncol(base_table))
  output <- capture.output(for (i in z){list <- cat(sum(base_table[,i]),"\n")})
  output <- as.numeric(output)

  namer <- colnames(base_table)
  names(output) <- namer

  nval_labs <- paste0(" \n(N = ", output, ")") %>%
    c("", ., "")

  return(nval_labs)
}

#Table percentage fix function  --------------------------------------
# Calculate Percentages for "select multiple" tables
table_fix <- function(table){
  z <- as.list(1:ncol(table))
for (i in z) {
  table[,i] <- (table[,i]/sum(table[,i]))*100
  }
return(table)
  }

# Sample size comment function  --------------------------------------
# Add comment on whether a question is 'select multiple' or 'select single'

#Single function
ar_survey_sample_sin <- "\u003a\u0639\u064a\u0646\u0629\u0020\u0627\u0644\u0645\u0633\u062d\u0020\u0627\u0644\u0625\u062c\u0645\u0627\u0644\u064a\u0629"
ar_mult_choices_sin <- "\u0647\u0630\u0627\u0020\u0627\u0644\u0633\u0624\u0627\u0644\u0020\u064a\u0633\u0645\u062d\u0020\u0641\u0642\u0637\u0020\u0628\u0625\u062c\u0627\u0628\u0629\u0020\u0648\u0627\u062d\u062f\u0629*"
ar_actual_sample_sin <- "\u0639\u064a\u0646\u0629\u0020\u0627\u0644\u0633\u0624\u0627\u0644\u0020\u0627\u0644\u0641\u0639\u0644\u064a\u0629\u0020\u0645\u0639\u0631\u0648\u0636\u0629\u0020\u0641\u064a\u0020\u0631\u0623\u0633\u0020\u0627\u0644\u062c\u062f\u0648\u0644*"

en_survey_sample_sin <- "Total survey respondents:"
en_mult_choices_sin <- "*This question allows for only one response"
en_actual_sample_sin <- "*The total responses for this question are displayed in the header"


.selection_note_single <- function(data, lang){
  if (lang == "ar"){
    note <- paste(ar_survey_sample_sin,
                  {paste0("N = ", nrow(data))},ar_mult_choices_sin, ar_actual_sample_sin,sep = "\n")}
  if (lang == "en"){
    note <- paste(en_survey_sample_sin,{paste0("N = ", nrow(data))},en_mult_choices_sin,en_actual_sample_sin, sep = "\n")}
  return(note)
}


#Multi function
ar_survey_sample_multi <- "\u003a\u0639\u064a\u0646\u0629\u0020\u0627\u0644\u0645\u0633\u062d\u0020\u0627\u0644\u0625\u062c\u0645\u0627\u0644\u064a\u0629"
ar_mult_choices_multi <- "\u0647\u0630\u0627\u0020\u0627\u0644\u0633\u0624\u0627\u0644\u0020\u064a\u0633\u0645\u062d\u0020\u0628\u0623\u0643\u062b\u0631\u0020\u0645\u0646\u0020\u0625\u062c\u0627\u0628\u0629\u0020\u0648\u0627\u062d\u062f\u0629*"
ar_actual_sample_multi <- "\u0639\u064a\u0646\u0629\u0020\u0627\u0644\u0633\u0624\u0627\u0644\u0020\u0627\u0644\u0641\u0639\u0644\u064a\u0629\u0020\u0645\u0639\u0631\u0648\u0636\u0629\u0020\u0641\u064a\u0020\u0631\u0623\u0633\u0020\u0627\u0644\u062c\u062f\u0648\u0644*"

en_survey_sample_multi <- "Total survey respondents:"
en_mult_choices_multi <- "*This question allows for more than one response"
en_actual_sample_multi <- "*The total responses for this question are displayed in the header"

.selection_note_multi <- function(data, lang){
  if (lang == "ar"){
    note <- paste(ar_survey_sample_multi,
                  {paste0("N = ", nrow(data))},ar_mult_choices_multi, ar_actual_sample_multi,sep = "\n")}
  if (lang == "en"){
    note <- paste(en_survey_sample_multi
                  ,{paste0("N = ", nrow(data))},en_mult_choices_multi,en_actual_sample_multi, sep = "\n")}
  return(note)
}
