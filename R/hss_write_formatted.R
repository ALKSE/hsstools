#' Write formatted tables to a list for selected variables
#'
#' @param df The dataframe containing relevant variables
#' @param questions A named character vector containing variable names and the table type required.
#' @param group The desired grouping/disaggregation variable.
#' @param percent Logical vector if the table should be created with percentage values. If set to FALSE, counts are shown.
#'
#' @return A named list of flextable objects
#' @export
#'
hss_write_formatted <- function(df, questions, group, percent = TRUE) {
  out <- lapply(questions, function(x) {
    tryCatch(
      if (names(questions[match(x, questions)]) == "select_one") {
        hss_table_single(df, x, group, percent) %>%
          hss_label(x) %>%
          hss_format_single()
      } else if (names(questions[match(x, questions)]) == "select_multiple") {
        hss_table_multi(df, x, group, percent) %>%
          hss_label(x) %>%
          hss_format_multi()
      },
      error = function(e) NULL
    )
  })
  names(out) <- questions
  return(out)
}
