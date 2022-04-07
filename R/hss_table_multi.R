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
  require(dplyr)
  require(stringr)
  resp <- hss_get_multi(var)
  sub_var <- hss_lookup_var(hss_lookup_list(var), 2, 8)
  if (is.na(sub_var)) {
    sub_q <- str_match(sub_var, "Q.+(?=\\})")
    sub_a <- str_match(sub_var, "(?<=\\')\\d{1,2}(?=\\')")
    df <- df %>% filter(.[hss_lookup_list(sub_q, TRUE)] == !!as.numeric(sub_a))
  }
  total <- if (percent == TRUE) {
    "mean"
  } else {
    "sum"
  }
  x <- addmargins(
    questionr::cross.multi.table(df[!is.na(resp[1]), resp],
                                 crossvar = df[!is.na(resp[1])][[group]],
                                 freq = percent,
                                 tfreq = "col",
                                 n = FALSE,
                                 na.rm = TRUE
    ),
    margin = 2,
    FUN = total
  )
  p <- hss_chisq(df, resp, group, full = FALSE)
  x <- cbind(
    "Answer" = rownames(x),
    as.data.frame.matrix(x, row.names = NULL),
    p
  )
  return(x)
}
