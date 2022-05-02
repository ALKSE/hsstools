#' Find 'select multiple' response variables associated with a question variable.
#'
#' This function returns all associated answer variables with
#' a 'select-multiple' question variable. As of now, the only way to retrieve these variables
#' is by inputting the "_all" variable name.
#'
#' @param var The variable for which 'select multiple' response variable are needed.
#' @param reverse If set to TRUE returns the new variable names. FALSE returns old names.
#'
#' @return a character string with all associated 'select multiple' variable names.
#' @export
#'
hss_get_multi <- function(var) {
  if (var %in% dict_var$name == TRUE) {
    var_old <- var
    var_new <- hss_lookup_list(var, reverse = TRUE)
  } else if (hss_lookup_list(var, reverse = FALSE) %in% dict_var$name == TRUE) {
    var_old <- hss_lookup_list(var, reverse = FALSE)
    var_new <- var
  } else {
    warning(var, " not in dictionary or lookup list.")
  }

  answers <- names(
    dplyr::select(
      df,
      starts_with(
        stringr::str_replace(var_new, "_all", "_")
      )
    )
  )
  return(answers)
}
