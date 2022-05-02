#' Create overview table for 'select-multiple' questions
#'
#' This creates overview tables for multiple questions that contain the same response options.
#' For example when the same question is repeated for different events. This implementation is specifically for
#' 'select-multiple' questions. For 'select-one' questions see \code{\link{hss_overview_single}}
#'
#' @param df The dataframe containing the questions.
#' @param vars A commom character string that is shared between all relevant variables.
#' @param percent Set to TRUE to display percentages, set to FALSE to display counts. Default is TRUE
#'
#' @return A dataframe with responses for the selected variables.
#' @export
#'
hss_overview_multi <- function(df, vars, percent = TRUE) {
  vars <- names(df %>% select(contains(vars))) %>%
    str_replace(paste(vars, "_.+", sep = ""), vars) %>%
    unique()

  if (percent == TRUE) {
    tables <- lapply(vars, function(x) {
      questionr::multi.table(df %>% select(starts_with(x)), freq = TRUE)[, 2] %>%
        sprintf("%1.2f%%", .)
    })
  } else if (percent == FALSE) {
    tables <- lapply(vars, function(x) {
      questionr::multi.table(df %>% select(starts_with(x)), freq = FALSE) %>%
        as.data.frame() %>%
        select(Freq)
    })
  } else {
    stop("Invalid input for percent:", percent)
  }

  tables <- as.data.frame(tables)
  names(tables) <- vars
  return(tables)
}
