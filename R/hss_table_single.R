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
  # retrieve old and new variable names
var <- .get_oldnew_varname(var)

# retrieve sub-setting variable and filter df

# tables
  if (percent == TRUE) {
    table <- table(forcats::as_factor(df[[var$new]]), forcats::as_factor(df[[group]])) %>%
      addmargins(margin = 2) %>%
      proportions(margin = 2) %>%
      addmargins(margin = 1)

    table <- as.data.frame(
      matrix(
        sprintf("%1.2f%%", table * 100),
        nrow(table),
        dimnames = dimnames(table)
      )
    )
    table <- dplyr::select(table, !contains("refused"))
  } else if (percent == FALSE) {
    table <- addmargins(
      addmargins(
        table(forcats::as_factor(df[[var$new]]), forcats::as_factor(df[[group]])),
        margin = 2
      ),
      margin = 1
    )
  } else {
    stop("Invalid input for percent:", percent)
  }
  table <- dplyr::bind_cols(
    !!var$new := rownames(table),
    as.data.frame.matrix(table, row.names = NULL),
  ) %>%
    dplyr::select(!contains("Refused"))

  rownames(table) <- NULL

  return(table)
}
