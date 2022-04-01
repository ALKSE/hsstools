#' Apply English or Arabic labels to a flextable object.
#' @rdname hsslabel
#' @description Apply English or Arabic text labels  to a flextable object.
#' hsslabel() calls upon hsslabel_var() and hsslabel_val() which return a
#' character string of length 1 containing the variable label, and a character
#' string of length n containing the value labels respectively.
#' @param table A flextable object.
#' @param variable The variable name
#' @param dict_var The dictionary object containing the variable names and labels.
#' @param dict_val The dictionary object containing the value names and labels.
#' @param lang The language to return. Accepts "en" or "ar".
#' @param lookup Set to TRUE to use a lookup table for renamed variables.
#' @param lookuplist If lookup is set to TRUE, provide object containing the lookup list.
#'
#' @return A flextable object with question and response labels.
#' @export
#'
#' @seealso [hssdict()] to create a variable or value label dictionary, [hssnewnames()] for creating a lookuplist of renamed variables.
#'
#' @examples
#' WIP
hsslabel <- function(table, variable, dict_var, dict_val, lang = "english", lookup = TRUE, lookuplist) {
  if(!is.na(hsslabel_var(variable, dict_var, lang, lookup, lookuplist)[1]) == TRUE) {
  table <- add_header_lines(table, values = hsslabel_var(variable, dict_var, lang, lookup, lookuplist))
  }
  if(!is.na(hsslabel_val(variable, dict_var, dict_val,lang, lookup, lookuplist)[1]) == TRUE) {
  table <-  mk_par(table, j = "Answer", value = as_paragraph(
    as_chunk(hsslabel_val(variable, dict_var, dict_val,lang, lookup, lookuplist)
    )
  )
  )
  }
  table
}

#' @rdname hsslabel
hsslabel_var <- function(variable, dict_var, lang = "english", lookup = TRUE, lookuplist) {
  variable <- if(lookup == TRUE) {hssnamelookup(lookuplist, variable)} else {variable}
  dplyr::filter(dict_var, name == variable) %>%
    dplyr::select(paste("label", lang, sep = "_")) %>%
    as.character()
}

#' @rdname hsslabel
hsslabel_val <- function(variable, dict_var, dict_val, lang = "english", lookup = TRUE, lookuplist) {
  variable <- if(lookup == TRUE) {hssnamelookup(lookuplist, variable)} else {variable}
  x <-   dplyr::filter(dict_var, name == variable) %>%
    dplyr::select(type) %>%
    as.character()
  dplyr::filter(dict_val, list_name == x) %>%
    dplyr::select(paste("label", lang, sep = "_")) %>%
    unlist(use.names = FALSE)
}
