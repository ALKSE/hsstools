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
hsstable_single <- function(df, var, group, percent = TRUE) {

# for determining sub-questions: this is determined in column 'relevant' of XLSform/dict_val
  # in de vorm ${Q0_1} = '2'. Kan hier vraagnummer en antwoordnummer uithalen!!

  if (percent == TRUE) {
    x <- proportions(
        addmargins(
          table(as_factor(df[[var]]), as_factor(df[[group]])),
          margin = 2
        ),
        margin = 2
      )
    x <- as.data.frame(
      matrix(
        sprintf("%.0f%%", x * 100),
        nrow(x),
        dimnames = dimnames(x)
      )
    )
   x <- dplyr::select(x, !contains("refused"))
  } else {
    x <-
      addmargins(
        table(df[[var]], df[[group]]),
        margin = 2
      )
  }
  x <- cbind(
    "Answer" = rownames(x),
    as.data.frame.matrix(x, row.names = NULL)
  )
  x
}

#' @param question The variable containing the sub-setting question
#' @param valid A character string containing responses that should be included in the contingency table
#' @rdname hsstable_single

hsstable_single_sub <- function(df, question, valid, var, group, percent = TRUE) {
  if (is.factor(df[[var]]) == FALSE | is.factor(df[[group]]) == FALSE) {
    warning("Not all selected variables are factors. Missing levels may not appear in table")
  }

  if (percent == TRUE) {
      proportions(
        addmargins(
          table(df[df[[question]] %in% valid, var], df[df[[question]] %in% valid, group]),
          margin = 2
        ),
        margin = 2
      )
  } else {
      addmargins(
        table(df[df[[question]] %in% valid, var], df[df[[question]] %in% valid, group]),
        margin = 2
      )
  }
}
