#' Runs chi-squared test on selected variables
#'
#' This functions runs chi-squared significance tests for one or more variables
#' in a dataframe with a single cross-variable. Variable names can be passed as a
#' single object or as a character string. hss_chisq_formatted returns a formatted character string
#' containing a verbose explanation of the p-value.
#'
#' @param df The dataframe containing the variable(s) of interest
#' @param var The variable(s) of interest. Accepts a single value or character string.
#' @param group The grouping (or disaggregation) variable.
#' @param full should the full results be returned. If set to FALSE, only p.value is returned
#' @param multi Set to FALSE if used for a 'select-one' question. Set to TRUE if used for a
#' 'select-multiple question to look up the corresponding response options.
#'
#' @return A vector containing the results of the chi-squared test for the selected
#' variables. If full is set to TRUE this will be a list, otherwise an atomic numeric vector.
#' For hss_chisq_formatted the output is a character vector of length 1.
#' @export
#' @examples
#' # Create dummy dictionary
#' dict_var <- dummy_var
#' dict_val <- dummy_val
#' # Calculte p-value for chi-squared test on a 'select-one' question.
#' hss_chisq(dummydata, "migr_nr", "gender")
#'
#' # Calculate p-value for chi-squared test on a 'select-multiple' question.
#' hss_chisq(dummydata, "migr_why_all", "gender", multi = TRUE)
#'
#' # Chi-squared test with full output
#' hss_chisq(dummydata, "migr_nr", full = TRUE)
#'
#'# Formatted output
#' hss_chisq_formatted(dummydata, "migr_nr", "gender")
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

#'@rdname hss_chisq
hss_chisq_formatted <- function(df, var, group) {

  chisq_formatted <- hss_chisq(df, var, group, full = FALSE, multi = FALSE) %>%
    paste(
      "Chi-squared is",
      if (is.na(.)) {
        "not applicable:"
      } else if (. >= 0.05) {
        "not significant:"
      } else {
        "significant:"
      },
      "p =",
      .
    )
  return(chisq_formatted)
}
