#' Runs chi-squared test on selected variables
#'
#' @param df The dataframe containing the variable(s) of interest
#' @param vars The variable(s) of interest. Accepts a single value or character string.
#' @param group The grouping (or disaggregation) variable.
#' @param full should the full results be returned. If set to FALSE, only p.value is returned
#'
#' @return A list or vector containing the results of the chi-squared test.
#' @export
#'
#' @examples
#' data <- testdata()
#' hsschisq(data, c("multi_option1", "multi_option2", "multi_option3"), "gender")
hsschisq <- function(df, vars, group, full = FALSE) {
  if (full == FALSE) {
    sapply(
      vars,
      function(x) {
        chisq.test(
          table(df[[x]], df[[group]])
        )[["p.value"]]
      }
    )
  } else {
    sapply(
      vars,
      function(x) {
        chisq.test(
          table(df[[x]], df[[group]])
        )
      }
    )
  }
}
