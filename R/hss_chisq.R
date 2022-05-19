#' Runs chi-squared test on selected variables
#'
#' This functions runs chi-squared significance tests for one or more variables
#' in a dataframe with a single cross-variable. Variable names can be passed as a
#' single object or as a character string.
#'
#' @param df The dataframe containing the variable(s) of interest
#' @param vars The variable(s) of interest. Accepts a single value or character string.
#' @param group The grouping (or disaggregation) variable.
#' @param full should the full results be returned. If set to FALSE, only p.value is returned
#'
#' @return A vector containing the results of the chi-squared test for the selected
#' variables. If full is set to TRUE this will be a list, otherwise an atomic numeric vector.
#' @export
#' @examples
#' # Generate dummy data
#' df <- hss_dummydata()
#'
#' # Calculte p-value for chi-squared test on a 'select-one' question.
#' pval_single <- hss_chisq(test, "singleresponse", "gender")
#'
#' # Calculate p-value for chi-squared test on a 'select-multiple' question.
#' multi_options <- c("multi_option1", "multi_option2", "multi_option3")
#' pval_multi <- hss_chisq(test, multi_options, "gender")
#'
#' # Chi-squared test with full output
#' full <- hss_chisq(f, "singleresponse", full = TRUE)
#'
hss_chisq <- function(df, var, group, full = FALSE, multi = FALSE) {
  if (!is.logical(full)) {
    stop("argument 'full' is not logical")
  }
  var <- .get_oldnew_varname(var)

  # subset dataframe by sub-seting variables
  df <- df %>% .subset_vars(var$new)

  # if used for select-multiple question, retrieve response options.
  if (multi == TRUE) {
    var$new <- .get_multi_valname(var$new)
  }

  # The call to chisq.test is wrapped in a trycatch() function to ensure the function
  # always returns something, even when provided unexpected input. Usually this occurs
  # when input is all zeroes.

  if (full == FALSE) {
    chisq_output <- sapply(
      var$new,
      function(var_element) {
        return(tryCatch(
          round(
            stats::chisq.test(df[[var_element]], df[[group]])[["p.value"]],
            digits = 3
          ),
          error = function(e) NA
        ))
      }
    )
  } else if (full == TRUE) {
    chisq_output <- lapply(
      var$new,
      function(var_element) {
        return(tryCatch(
          stats::chisq.test(df[[var_element]], df[[group]]),
          error = function(e) NA
        ))
      }
    )
  }
  return(chisq_output)
}
