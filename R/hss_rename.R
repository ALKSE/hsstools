#' dplyr::rename HSS variables
#'
#' This function is used to dplyr::rename the variables in the HSS dataset from the old
#' Q... names to human-readable names. These new names are expected to be present
#' in the XLS form for the dataset (both the variable names, and the suffixes for
#' dplyr::select-multiple answer options.). The function also removes any non-question columns
#' (notes etc.)
#' @param df The dataframe containing the HSS survey questions
#' @param dict_var Dictionary object of variable names created from the survey's XLS form
#' @param dict_val Dictionary obect of value names created from the survey's XLS form.
#'
#' @seealso `hss_create_dict()`
#'
#' @return Returns a dataframe with names updated to new column names and non-question
#' columns removed.
#' @export
hss_rename <- function(df, dict_var, dict_val) {
  # note: this function is based on a rather extensive R script that could probably
  # refactored to be a lot more concise. Due to time constraints, for now the script
  # has just been divided into several functions with this hss_rename function as a
  # convenient wrapper.
  out <- df %>%
    dplyr::rename.select_one(dict_var) %>%
    dplyr::rename.select_multiple(dict_val) %>%
    dplyr::rename.select_multiple_repeat(dict_var, dict_val) %>%
    drop.notes(dict_var)
  return(out)
}
#' @keywords internal
rename.select_one <- function(data, dict_var) {
  # dplyr::filter for all questions that have corresponding r_name
  dvar_filtered <- dplyr::filter(dict_var, !is.na(r_name))
  # match old & new name
  name_match <- match(dvar_filtered$name, names(data))
  names(data)[na.omit(name_match)] <- dvar_filtered$r_name[!is.na(name_match)]

  return(data)

}
#' @keywords internal
filter.dval <- function(dict_val) {
  filtered <- dict_val %>%
    dplyr::select(-label_english, -label_stata, -r_table_label_en, -location) %>%
    dplyr::filter(
      !is.na(r_name),
      is.na(r_name_prefix)
    ) %>%
    dplyr::mutate(
      varname = with(dict_var, name[match(list_name, type)])
    ) %>%
    dplyr::select(varname, everything(), -list_name) %>%
    dplyr::mutate(
      name_new = stringr::str_c(varname, "/", name)
    ) %>%
    dplyr::select(varname, name, name_new, everything())
  return(filtered)
}

list.renamed <- function(dict_val) {
  dval_filtered <- dplyr::filter.dval(dict_val)
  list_done <- dval_filtered %>%
    dplyr::select(varname) %>%
    dplyr::rename(name = varname) %>%
    dplyr::mutate(
      status = "done"
    )
  return(list_done)
}
#' @keywords internal
rename.select_multiple <- function(data, dict_val) {

  filtered <- dict_val %>%
    dplyr::select(-label_english, -label_stata, -r_table_label_en, -location) %>%
    dplyr::filter(
      !is.na(r_name),
      is.na(r_name_prefix)
    ) %>%
    dplyr::mutate(
      varname = with(dict_var, name[match(list_name, type)])
    ) %>%
    dplyr::select(varname, everything(), -list_name) %>%
    dplyr::mutate(
      name_new = stringr::str_c(varname, "/", name)
    ) %>%
    dplyr::select(varname, name, name_new, everything())

  # dplyr::rename vars using sheet2
  name_match <- match(filtered$name_new, names(data))
  names(data)[na.omit(name_match)] <- filtered$r_name[!is.na(name_match)]

  return(data)
}
#' @keywords internal
rename.select_multiple_repeat <- function(data, dict_var, dict_val) {
  list_done <- list.renamed(dict_val)

  dval_filtered <- dict_val %>%
    dplyr::select(-list_name, -label_english, -label_stata, -r_table_label_en, -location) %>%
    dplyr::filter(!is.na(r_name_prefix)) %>%
    tidyr::separate_rows(r_name_prefix, sep = " ") %>%
    dplyr::mutate(
      r_name2 = r_name,
      r_name_middle2 = r_name_middle,
      r_name_prefix2 = r_name_prefix
    ) %>%
    tidyr::unite(r_name, c(r_name_prefix, r_name_middle, r_name), sep = "", na.rm = TRUE) %>%
    dplyr::rename(
      r_name_suffix = r_name2,
      r_name_middle = r_name_middle2,
      r_name_prefix = r_name_prefix2
    ) %>%
    dplyr::relocate(r_name_suffix, .before = r_name_prefix) %>%
    dplyr::relocate(r_name_prefix, .before = r_name_middle) %>%
    dplyr::mutate(
      r_name_base = r_name_prefix
    ) %>%
    tidyr::unite(r_name_base, c(r_name_prefix, r_name_middle), sep = "", na.rm = TRUE) %>%
    dplyr::rename(
      r_name_val = r_name
    )


  dvar_filtered <- dict_var %>%
    dplyr::select(name, r_name) %>%
    dplyr::filter(
      !is.na(r_name)
    ) %>%
    dplyr::mutate(
      r_name_all = r_name,
      r_name2 = r_name
    ) %>%
    dplyr::left_join(list_done, by = "name") %>%
    dplyr::filter(is.na(status)) %>%
    dplyr::select(-status) %>%
    dplyr::mutate(
      r_name_all_temp = stringr::str_ends(r_name, "_all")
    ) %>%
    dplyr::filter(r_name_all_temp == TRUE) %>%
    dplyr::select(-r_name_all_temp) %>%
    dplyr::mutate(
      r_name_all = str_remove(r_name_all, "_all")
    ) %>%
    dplyr::select(-r_name2) %>%
    dplyr::rename(
      name_var = name,
      r_name_base = r_name_all
    ) %>%
    dplyr::ful_join(dval_filtered, by = "r_name_base") %>%
    dplyr::mutate(
      varname = paste(name_var, "/", name, sep = "")
    ) %>%
    dplyr::select(varname, r_name_val)


  name_match <- match(dvar_filtered$varname, names(data))
  names(data)[na.omit(name_match)] <- dvar_filtered$r_name_val[!is.na(name_match)]
  return(data)
}
#' @keywords internal
drop.notes <- function(data, dict_var) {
  dvar_filtered <- dict_var %>%
    dplyr::select(type, name) %>%
    dplyr::filter(type=="note") %>%
    dplyr::select(name)

  data <- data[ , !(names(data) %in% dvar_filtered$name)]

  return(data)
}
