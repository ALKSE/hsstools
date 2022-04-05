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
hss_table_single <- function(df, var, group, percent = TRUE) {

  # for determining sub-questions: this is determined in column 'relevant' of XLSform/dict_val
  # in de vorm ${Q0_1} = '2'. Kan hier vraagnummer en antwoordnummer uithalen!!

  if (percent == TRUE) {
    sub_var <- hss_lookup_var(hss_lookup_list(variable), 2, 8)
    if (is.na(sub_var)) {
      sub_q <- str_match(sub_var, "Q.+(?=\\})")
      sub_a <- str_match(sub_var, "(?<=\\')\\d{1,2}(?=\\')")
      df <- df %>% filter(.[hss_lookup_list(sub_q, TRUE)] == !!as.numeric(sub_a))
    }
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
  } else if (percent == FALSE) {
    i <- hss_lookup_var(hss_lookup_list(variable), 2, 8)
    if (is.na(i)) {
      q <- str_match(i, "Q.+(?=\\})")
      a <- str_match(i, "(?<=\\')\\d{1,2}(?=\\')")
      df <- df %>% filter(.[hss_lookup_list(q, TRUE)] == !!as.numeric(a))
    }
    x <-
      addmargins(
        table(df[[var]], df[[group]]),
        margin = 2
      )
  } else {
    stop("Invalid input for percent :", percent)
  }
  x <- cbind(
    "Answer" = rownames(x),
    as.data.frame.matrix(x, row.names = NULL)
  )
  x
}
