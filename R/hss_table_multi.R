#' Generate contingency table for multiresponse questions
#'
#' @param df The dataframe containing the multiresponse questions
#' @param resp A character string of all response variables to include
#' @param group A grouping (or disaggregation) variable.
#' @param percent Set to TRUE to show percentages. Set to FALSE to show counts
#' @param digits The number of (significant) digits to display. Trailing zeroes are
#' always removed. Note that 'digits' does not mean 'decimals', so digits = 3 will display as 'mm.d' not 'mm.ddd'
#'
#' @return A contingency table containing the multiresponse answers and a grouping variable
#' @export
hss_table_multi <- function(df, var, group, percent = TRUE, digits = 1) {
  # get scipen default options, then set to make sure no scientific notation is used

  # retrieve old&new variable name, and retrieve response options.
  var <- .get_oldnew_varname(var)
  resp <- .get_multi_valname(var$new, df)

  # retrieve sub-setting variable and filter df
  df <- df %>% .subset_vars(var$new)

  # set options depending on percentage/count setting.
  Total <- if (percent == TRUE) {
    "mean"
  } else if (percent == FALSE) {
    "sum"
  }

  # create contingency table with 'Total' column
  table <- addmargins(
    questionr::cross.multi.table(df[!is.na(df[eval(resp[1])]), resp],
      crossvar = forcats::as_factor(df[!is.na(df[eval(resp[1])]), ][[group]]),
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
      signif(digits = digits) %>%
      sprintf("%s%%", .) %>%
      matrix(
        nrow(table),
        dimnames = dimnames(table)
      ) %>%
      as.data.frame()
  }
  # calculate p values for each response option.
  p <- hss_chisq(df, var$new, group, full = FALSE, multi = TRUE)
  # add row names as columns and convert to dataframe. P values added as column
  table <- dplyr::bind_cols(
    Response = rownames(table),
    as.data.frame.matrix(table, row.names = NULL),
    "p" = p
  )

  rownames(table) <- NULL

  # apply N value labels to column headers
  names(table) <- paste0(
    names(table),
    .get_nval_multi(df, var$new, group)
  )
return(table)
}
