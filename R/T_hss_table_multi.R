#' Create contingency table for multiresponse question
#'
#' Create contingency tables for HSS questions with multiple response options and
#' a cross-variable. This function requires only a single question variable as input
#' and looks up the corresponding response variables. Tables can be created with
#' either counts or percentages and the number of significant digits for percentages
#' can be set. Tables created with this function come with a couple of additions
#' according to HSS preferences. Most notably: sums are added over columns, N-values
#' are added to column headers, and a column containing p-values for chi-squared
#' statistical test for each response option is added.
#'
#' @param df The dataframe containing the multiresponse questions
#' @param var The name of the question variable.
#' @param group A grouping (or disaggregation) variable.
#' @param percent Set to TRUE to show percentages. Set to FALSE to show counts
#' @param digits The number of (significant) digits to display. Trailing zeroes are
#' always removed. Note that 'digits' does not mean 'decimals', so digits = 3 will display as 'mm.d' not 'mm.ddd'
#'
#' @return A contingency table containing the multiresponse answers and a grouping variable
#' @export
T_hss_table_multi <- function(df, var, group, percent = TRUE, digits = 1) {
  # retrieve response options.
  resp <- .get_multi_valname(var, df)

  # set options depending on percentage/count setting.
  Total <- if (percent == TRUE) {
    "mean"
  } else if (percent == FALSE) {
    "sum"
  }

  # create contingency table with 'Total' column
  table <- addmargins(
    questionr::cross.multi.table(df[!is.na(df[eval(resp[1])]), resp],
      crossvar = haven::as_factor(df[!is.na(df[eval(resp[1])]), ][[group]]),
      digits = digits,
      freq = percent,
      tfreq = "col",
      n = FALSE,
      na.rm = TRUE
    ),
    margin = 2,
    FUN = Total
  )
  # formats percentages to show specified digits and adds %sign. (all values are
  # converted to character)
  if (percent == TRUE) {

    table <- table %>%
      formatC(digits = digits, format = "fg", mode = "real") %>%
      as.double() %>%
      sprintf("%s%%", .)%>%
      matrix(
        nrow(table),
        dimnames = dimnames(table)
      ) %>%
      as.data.frame()
  }
  # calculate p values for each response option.
  p <- hss_chisq(df, var, group, full = FALSE, multi = TRUE)
  # add row names as columns and convert to dataframe. P values added as column
  table <- dplyr::bind_cols(
    !!var := rownames(table),
    as.data.frame.matrix(table, row.names = NULL),
    "p" = p
  )

  rownames(table) <- NULL

  # apply N value labels to column headers
  names(table) <- paste0(
    names(table),
    .get_nval_multi(df, var, group)
  )
return(table)
}
