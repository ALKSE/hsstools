#' Runs chi-squared test on selected variables
#'
#' This functions runs chi-squared significance tests for one or more variables
#' in a dataframe, with a single cross-variable. Variable names are passed as a
#' character vector containing one or more variable names. hss_chisq_formatted
#' returns a formatted character string containing a verbose description of the p-value.
#' This verbose output is used in formatted data tables.
#'
#' @param df The dataframe containing the variable(s) of interest
#' @param var The names of the variable(s) of interest. Accepts a single value or a character vector
#' containing multiple variable names.
#' @param group The grouping (or disaggregation) variable.
#' @param full TRUE/FALSE if the full results should be returned. Set to FALSE by
#' default, returning only the p.value.
#' @param multi Set to FALSE if used for a 'select-one' question. Set to TRUE if used for a
#' 'select-multiple question to look up the corresponding response options.
#'
#' @return A vector containing the results of the chi-squared test for the selected
#' variables. If full is set to TRUE this will be a list, otherwise an atomic numeric vector.
#' For hss_chisq_formatted the output is a character vector.
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
T_hss_chisq <- function(df, var, group, full = FALSE, multi = FALSE) {
  if (!is.logical(full)) {
    stop("argument 'full' is not logical")
  }

  # if used for select-multiple question, retrieve response options.
  if (multi == TRUE) {
    var <- .get_multi_valname(var, df)
  }

  # The call to chisq.test is wrapped in a trycatch() function to ensure the function
  # always returns something, even when provided unexpected input. Usually this occurs
  # when input is all zeroes.

  if (full == FALSE) {
    chisq_output <- sapply(
      var,
      function(var_element) {
        return(tryCatch(
          signif(
            stats::chisq.test(df[[var_element]], df[[group]])[["p.value"]],
            digits = 3
          ),
          error = function(e) NA
        ))
      }
    )
  } else if (full == TRUE) {
    chisq_output <- lapply(
      var,
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

#'@rdname T_hss_chisq
T_hss_chisq_formatted <- function(df, var, group) {
  chisq <- T_hss_chisq(df, var, group, full = FALSE, multi = FALSE)
  chisq_formatted <- paste(
      "Chi-squared is",
      if (is.na(chisq)) {
        "not applicable:"
      } else if (chisq >= 0.05) {
        "not significant:"
      } else {
        "significant:"
      },
      "p",
      if (is.na(chisq)) {paste0("= NA")}
      else if (chisq > 0.0005) { paste0("= ", round(chisq, digits = 3))} else {"< 0.001"}
    )
  return(chisq_formatted)
}
