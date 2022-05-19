#' Generate contingency table for multiresponse questions
#'
#' @param df The dataframe containing the multiresponse questions
#' @param resp A character string of all response variables to include
#' @param group A grouping (or disaggregation) variable.
#' @param percent Set to TRUE to show percentages. Set to FALSE to show counts
#'
#' @return A contingency table containing the multiresponse answers and a grouping variable
#' @export
hss_table_multi <- function(df, var, group, percent = TRUE) {
  # retrieve old&new variable name, and retrieve response options.
  var <- .get_oldnew_varname(var)
  resp <- .get_multi_valname(var[["new"]])

  # retrieve sub-setting variable and filter df
  df <- df %>% .subset_vars(var$new)

  # set options depending on percentage/count setting.
  total <- if (percent == TRUE) {
    "mean"
  } else if (percent == FALSE) {
    "sum"
  }

  # create contingency table with 'total' column
  table <- addmargins(
    questionr::cross.multi.table(df[!is.na(df[eval(resp[1])]), resp],
      crossvar = forcats::as_factor(df[!is.na(df[eval(resp[1])]), ][[group]]),
      digits = 2,
      freq = percent,
      tfreq = "col",
      n = FALSE,
      na.rm = TRUE
    ),
    margin = 2,
    FUN = total
  )
  # convert proportions to percentages
  if (percent == TRUE) {
    table <- as.data.frame(
      matrix(
        sprintf("%1.2f%%", table),
        nrow(table),
        dimnames = dimnames(table)
      )
    )
  }
  # calculate p values for each response option.
  p <- hss_chisq(df, var$new, group, full = FALSE, multi = TRUE)
  # add row names as columns and convert to dataframe. P values added as column
  table <- bind_cols(
    !!var$new := rownames(table),
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
