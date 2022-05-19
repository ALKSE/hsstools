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
  df <- df %>% .subset_vars(var$new)

  # create table
  if (percent == TRUE) {
    # creates contingency table with 'total' column
    table <- table(
      forcats::as_factor(df[[var$new]]),
      forcats::as_factor(df[[group]])
    ) %>%
      addmargins(margin = 2) %>%
      proportions(margin = 2)
    # converts values from proportions to percentage
    table <- as.data.frame(
      matrix(
        sprintf("%1.2f%%", table * 100),
        nrow(table),
        dimnames = dimnames(table)
      )
    )
    # removes 'refused to answer' category from grouping variable if present
    table <- dplyr::select(table, !contains("refused"))
  } else if (percent == FALSE) {
    # create contingency table with 'total' column
    table <- table(
      forcats::as_factor(df[[var$new]]),
      forcats::as_factor(df[[group]])
    ) %>%
      addmargins(margin = 2)
  } else {
    stop("Invalid input for percent:", percent)
  }

  # add rownames (response options) as column and convert to dataframe
  table <- dplyr::bind_cols(
    !!var$new := rownames(table),
    as.data.frame.matrix(table, row.names = NULL),
  ) %>%
    dplyr::select(!contains("Refused"))

  rownames(table) <- NULL

  # apply N value labels to column headers
  names(table) <- paste0(
    names(table),
    .get_nval_single(df, var$new, group)
  )

  return(table)
}
