#' Runs chi-squared test on selected variables
#'
#' @param df The dataframe containing the variable(s) of interest
#' @param vars The variable(s) of interest. Accepts a single value or character string.
#' @param group The grouping (or disaggregation) variable.
#' @param full should the full results be returned. If set to FALSE, only p.value is returned
#'
#' @return A vector containing the results of the chi-squared test for the selected variables.
#' @export
#'
hss_chisq <- function(df, vars, group, full = FALSE) {


  if (full == FALSE) {
    x <- sapply(
      vars,
      function(x) {
        return(tryCatch(

        if(sum(!is.na(df[[x]])) > 0) {
          round(
        chisq.test(
          table(df[[x]], df[[group]])
        )[["p.value"]], digits = 3)
        } else {0}, error = function(e) 0))
      }
    )
  } else {
    x <- sapply(
      vars,
      function(x) {
        return(tryCatch(
          if(sum(!is.na(df[[x]])) > 0) {
            chisq.test(
              table(df[[x]], df[[group]])
            )
          } else {0}, error = function(e) 0))
      }
    )
  }
  return(x)
}
