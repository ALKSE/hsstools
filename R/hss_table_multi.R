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
  if (var %in% dict_var$name == TRUE) {
    var_old <- var
    var_new <- hss_lookup_list(var, reverse = TRUE)
  } else if (hss_lookup_list(var, reverse = FALSE) %in% dict_var$name == TRUE) {
    var_old <- hss_lookup_list(var, reverse = FALSE)
    var_new <- var
  } else {
    warning(var, " not in dictionary or lookup list.")
  }

  resp <- hss_get_multi(var_old)

  sub_var <- hss_lookup_var(var_old, 2, 8)

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
  } else {
    "sum"
  }
  x <- addmargins(
    questionr::cross.multi.table(df[!is.na(resp[1]), resp],
      crossvar = forcats::as_factor(df[!is.na(resp[1])][[group]]),
      digits = 2,
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
