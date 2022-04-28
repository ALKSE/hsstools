#' Get overview table for 'select-multiple' questions
#'
#' @param df The dataframe containing the questions
#' @param vars Selected based on a string that identifies all relevant variables
#' @param percent T/F
#'
#' @return
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
