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

  resp <- .get_multi_valname(var$new)

  sub_var <- hss_lookup_var(var$old, 2, 8)

  if (!is.na(sub_var)) {
    sub_q <- stringr::str_extract_all(sub_var, "Q.{1,5}(?=\\})") %>%
      unlist() %>%
      stringr::str_split(" ") %>%
      unlist()
    sub_a <- stringr::str_extract_all(sub_var, "(?<=\\')\\d{1,2}(?=\\')") %>%
      unlist() %>%
      stringr::str_split(" ") %>%
      unlist()
    df <- df %>% dplyr::filter(.[hss_lookup_list(sub_q, TRUE)] == !!as.numeric(sub_a))
  }

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

  table <- cbind(
    "Answer" = rownames(table),
    as.data.frame.matrix(table, row.names = NULL),
    p
  )

  rownames(table) <- NULL

  return(table)
}
