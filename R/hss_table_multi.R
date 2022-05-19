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
  var <- .get_oldnew_varname(var)
  resp <- .get_multi_valname(var[["new"]])

  total <- if (percent == TRUE) {
    "mean"
  } else if (percent == FALSE) {
    "sum"
  }

  table <- addmargins(
    questionr::cross.multi.table(df[!is.na(df[eval(resp[1])]), resp],
      crossvar = forcats::as_factor(df[!is.na(df[eval(resp[1])]),][[group]]),
      digits = 2,
      freq = percent,
      tfreq = "col",
      n = FALSE,
      na.rm = TRUE
    ),
    margin = 2,
    FUN = total
  )

  if(percent == TRUE) {
    table <- as.data.frame(
    matrix(
      sprintf("%1.2f%%", table),
      nrow(table),
      dimnames = dimnames(table)
    )
    )
  }

  p <- hss_chisq(df, resp, group, full = FALSE)

  table <- bind_cols(
    !!var$new := rownames(table),
    as.data.frame.matrix(table, row.names = NULL),
    "p" = p
  )

  rownames(table) <- NULL

  return(table)
}
