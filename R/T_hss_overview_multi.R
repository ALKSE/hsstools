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
T_hss_overview_multi <- function(df, vars, percent = TRUE) {
  grps_regex <- paste0(vars, "_.+") %>% stringr::str_replace_all("__", "_")
  grps <- names(df %>% dplyr::select(dplyr::matches(vars))) %>%
    stringr::str_replace_all(grps_regex, vars) %>%
    unique()
    resp_regex <- paste0(".+_", vars) %>% stringr::str_replace_all("__", "_")
  resp <- names(df %>% dplyr::select(contains(vars))) %>%
    stringr::str_replace(paste(".+_", vars, sep = ""), vars) %>%
    unique()

  if (percent == TRUE) {
    tables <- lapply(grps, function(x) {
      questionr::multi.table(df %>% dplyr::select(dplyr::starts_with(x)) %>%
                               dplyr::filter(rowSums(is.na(.)) != ncol(.)),
                             freq = TRUE)[, 2] %>%
        sprintf("%1.2f%%", .)
    })
  } else if (percent == FALSE) {
    tables <- lapply(grps, function(x) {
      questionr::multi.table(df %>% dplyr::select(dplyr::starts_with(x)), freq = FALSE) %>%
        as.data.frame() %>%
        dplyr::select(Freq)
    })
  } else {
    stop("Invalid input for percent:", percent)
  }

  tables <- dplyr::bind_cols(resp,
                  as.data.frame(tables)
  )
  names(tables) <- c("answer", grps)
  return(tables)
}
