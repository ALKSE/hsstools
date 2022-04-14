#' Create overview table
#'
#' @param df the data frame to use
#' @param vars a character vector with relevant variable strings.
#' @param type 'single' or 'multi'
#' @return a dataframe
#' @export
#'
hss_table_overview <- function(df, vars, type) {
  table <- lapply(vars, function(x) {
    if (type == "multi") {
      df %>%
        dplyr::select(contains(x)) %>%
        dplyr::filter(if_all(everything(), ~ !is.na(.x))) %>%
        dplyr::summarise(across(.fns = sum, na.rm = TRUE)) %>%
        t()
    } else if (type == "single") {
      df %>%
        dplyr::select(rlang::sym(x)) %>%
        as_factor() %>%
        dplyr::filter(if_all(everything(), ~ !is.na(.x))) %>%
        group_by(!!rlang::sym(x), .drop = FALSE) %>%
        summarise(n()) %>%
        select(-rlang::sym(x))
    } else {
      warning(type, " is not a valid table type.")
    }
  }) %>%
    bind_cols()
  names(table) <- vars
  return(table)
}
