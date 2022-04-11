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
  require(dplyr)
  require(stringr)

  if (var %in% dict_var$name == TRUE) {
    var_old <- var
    var_new <- hss_lookup_list(var, reverse = TRUE)
  } else if (hss_lookup_list(var, reverse = FALSE) %in% dict_var$name == TRUE) {
    var_old <- hss_lookup_list(var, reverse = FALSE)
    var_new <- var
  } else {
    warning(var, " not in dictionary or lookup list.")
  }

    sub_var <- hss_lookup_var(var_old, 2, 8)
    if (!is.na(sub_var) & !is.null(sub_var)) {
      sub_q <- str_match(sub_var, "Q.+(?=\\})")
      sub_a <- str_match(sub_var, "(?<=\\')\\d{1,2}(?=\\')")
      df <- df %>% filter(.[hss_lookup_list(sub_q, TRUE)] == !!as.numeric(sub_a))
    }
    if (percent == TRUE) {
    x <- table(as_factor(df[[var_new]]), as_factor(df[[group]])) %>%
      addmargins(margin = 2) %>%
      proportions(margin = 2) %>%
      addmargins(margin = 1)

    x <- as.data.frame(
      matrix(
        sprintf("%1.2f%%", x * 100),
        nrow(x),
        dimnames = dimnames(x)
      )
    )
    x <- dplyr::select(x, !contains("refused"))
  } else if (percent == FALSE) {
    x <- addmargins(
      addmargins(
        table(as_factor(df[[var_new]]), as_factor(df[[group]])),
        margin = 2
      ),
      margin = 1
    )
  } else {
    stop("Invalid input for percent:", percent)
  }
  x <- cbind(
    "Answer" = rownames(x),
    as.data.frame.matrix(x, row.names = NULL)
  )
  return(x)
}
