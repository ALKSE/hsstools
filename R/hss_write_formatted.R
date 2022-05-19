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
  out <- lapply(questions, function(questions_element) {
    tryCatch(
      if (names(questions[match(questions_element, questions)]) == "select_one") {
        hss_table_single(df, questions_element, group, percent) %>%
          hss_label(questions_element) %>%
          hss_format_single() %>%
          flextable::add_footer_lines(
            values = hss_chisq_formatted(df, questions_element, group)
          )
      } else if (names(questions[match(questions_element, questions)]) == "select_multiple") {
        hss_table_multi(df, questions_element, group, percent) %>%
          hss_label(questions_element) %>%
          hss_format_multi()
      },
      error = function(e) NULL
    )
  })
  names(out) <- questions
  return(out)
}
