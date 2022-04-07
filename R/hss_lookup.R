#' Helper functions to look up variable or value names and labels.
#'
#' @description These helper functions are used to look up info related to a
#' specific variable. This can be the variable label, the associated value labels
#' or the old/new variable name. These functions are mostly for use within other
#' functions.
#'
#' @param var The variable name. Only a single character value is accepted, except for hss_lookup_list, which accepts a character string.
#' @param input_col The column in which the variable name can be found
#' @param return_col The column for which the associated value should be returned
#' @param reverse If set to TRUE will return the new name instead of the old name. Set to FALSE by default.
#'
#' @return A character string containing the found value(s)
#' @export
#'
#' @rdname hss_lookup
hss_lookup_var <- function(var, input_col, return_col) {
  require(dplyr)
  if(var %in% dict_var[[input_col]] == FALSE) {
    stop("variable name \'", var, "\' not present in dict_var column: \'", names(dict_var[input_col]), "\'.")
  }
  dict_var %>% dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(return_col) %>%
    unlist(use.names = FALSE)
}
#' @rdname hss_lookup
hss_lookup_val <- function(var, input_col, return_col) {
  require(dplyr)
  if(var %in% dict_val[[input_col]] == FALSE) {
    stop("variable name \'", var, "\' not present in dict_val column: \'", names(dict_val[input_col]), "\'.")
  }
    dict_val %>% dplyr::filter(.[[input_col]] == var) %>%
    dplyr::select(return_col) %>%
    unlist(use.names = FALSE)
}
#' @rdname hss_lookup
hss_lookup_list <- function(var, reverse = FALSE) {
  pos <- if(reverse == TRUE) {2} else {1}
  unlist(lapply(var, function(y) unlist(lookuplist[sapply(lookuplist, function(x) y %in% x)])[pos]))
}
