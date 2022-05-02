#' Create overview table for a group of similar 'select-one' questions
#'
#' @param df The dataframe containing the questions
#' @param vars Selected based on a string that identifies all relevant variables
#' @param percent T/F
#'
#' @return A dataframe with the selected questions.
#' @export
#'
hss_overview_single <- function(df, vars, percent = TRUE) {
  library(dplyr)
  library(stringr)
  vars <- names(df %>% select(contains(vars) & !contains("_what")))

  if (percent == TRUE) {
    tables <- lapply(vars, function(x) {
      table(forcats::as_factor(df[[x]])) %>%
        proportions() %>%
        addmargins(margin = 1) %>%
        as.data.frame() %>%
        select(Freq)
    })
  } else if (percent == FALSE) {
    tables <- lapply(vars, function(x) {
      table(forcats::as_factor(df[[x]])) %>%
        addmargins(margin = 1) %>%
        as.data.frame() %>%
        select(Freq)
    })
  } else {
    stop("Invalid input for percent:", percent)
  }
  labels <- c(levels(forcats::as_factor(df[[vars[[1]]]])), "Total")

  tables <- bind_cols(labels, tables)

  names(tables) <- c("Answer", vars)

  return(tables)
}
