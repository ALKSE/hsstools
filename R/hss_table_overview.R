#' Create overview table
#'
#' @param vars a character vector with relevant variable strings.
#'
#' @return a dataframe
#' @export
#'
hss_table_overview <- function(vars) {
  table <- lapply(vars, function(x) {
    df %>%
      select(contains(x)) %>%
      summarise(across(.fns = sum, na.rm = TRUE)) %>%
      t()
  }) %>%
    bind_cols()

  names(table) <- vars
  return(table)
}
