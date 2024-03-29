#' Create overview table for a group of similar 'select-one' questions
#'
#' This creates overview tables for multiple questions that contain the same response options.
#' For example when the same question is repeated for different events. This implementation is specifically for
#' 'select-one' questions. For 'select-multiple' questions see \code{\link{T_hss_overview_multi}}
#'
#' @param df The dataframe containing the questions
#' @param vars A commom character string that is shared between all relevant variables.
#' @param percent Set to TRUE to display percentages, set to FALSE to display counts. Default is TRUE
#'
#' @return A dataframe with responses for the selected variables.
#' @export
#'
T_hss_overview_single <- function(df, vars, percent = TRUE) {
  vars <- names(df %>% dplyr::select(dplyr::matches(vars) & !dplyr::contains("_what")))

  if (percent == TRUE) {
    tables <- lapply(vars, function(x) {
      table(forcats::as_factor(df[[x]]), useNA = "no") %>%
        proportions() %>%
        addmargins(margin = 1) %>%
        sprintf("%1.2f%%", .)
    })

  } else if (percent == FALSE) {
    tables <- lapply(vars, function(x) {
      table(forcats::as_factor(df[[x]]), useNA = "no") %>%
        addmargins(margin = 1) %>%
        as.data.frame() %>%
        select(Freq)
    })
  } else {
    stop("Invalid input for percent:", percent)
  }
  labels <- c(levels(forcats::as_factor(df[[vars[[1]]]])), "Total")

  tables <- dplyr::bind_cols(labels, tables)

  names(tables) <- c("Answer", vars)

  return(tables)
}
