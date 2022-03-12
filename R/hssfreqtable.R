#' HSS Data Table Generation
#'
#' @param df A dataframe containing the variable of interest and grouping variable.
#' @param var A character string with the variable name of interest.
#' @param group A character string with the grouping (or disaggregation) variable.
#' @param percent Set to TRUE to show percentages. Set to FALSE to show counts.
#'
#' @return A contingency table with the variable of interest and grouping variable.
#' @export
#'
#' @examples
#' data <- testdata()
#' hssfreqtable(data, "singleresponse", "gender")
hssfreqtable <- function(df, var, group, percent = TRUE) {
  if (is.factor(df[[var]]) == FALSE | is.factor(df[[group]]) == FALSE) {
    warning("Not all selected variables are factors. Missing levels may not appear in table")
  }

  if (percent == TRUE) {
    addmargins(
      proportions(
        addmargins(
          table(df[[var]], df[[group]]),
          margin = 2
        ),
        margin = 2
      ),
      margin = 1
    )
  } else {
    addmargins(
      addmargins(
        table(data[["singleresponse"]], data[["gender"]]),
        margin = 2
      ),
      margin = 1
    )
  }
}
