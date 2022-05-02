#' Create overview table for a group of similar 'select-one' questions
#'
#' This creates overview tables for multiple questions that contain the same response options.
#' For example when the same question is repeated for different events. This implementation is specifically for
#' 'select-one' questions. For 'select-multiple' questions see \code{\link{hss_overview_multi}}
#'
#' @param df The dataframe containing the questions
#' @param vars A commom character string that is shared between all relevant variables.
#' @param percent Set to TRUE to display percentages, set to FALSE to display counts. Default is TRUE
#'
#' @return A dataframe with responses for the selected variables.
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
