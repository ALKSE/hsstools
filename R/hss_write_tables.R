#' Write tables to a list for selected variables
#'
#' @param df The dataframe containing relevant variables
#' @param questions A named character vector containing variable names and the table type required.
#' @param group The desired grouping/disaggregation variable.
#' @param percent Logical vector if the table should be created with percentage values. If set to FALSE, counts are shown.
#'
#' @return A named list of dataframes.
#' @export
#'
hss_write_tables <- function(df, questions, group, percent = TRUE) {
  output_list <- lapply(questions, function(questions_element) {
    tryCatch(

      if (names(questions[match(questions_element, questions)]) == "select_one") {
        table <- list(
          table = hss_table_single(df, questions_element, group, percent = percent),
          p = hss_chisq(df, questions_element, group) %>%
            paste(
              "Chi-squared is",
              if (is.na(.)) {
                "not applicable:"
              } else if (. >= 0.05) {
                "not significant:"
              } else {
                "significant:"
              },
              "p =",
              .
            )
        )
      } else if (names(questions[match(questions_element, questions)]) == "select_multiple") {
        table <- hss_table_multi(df, questions_element, group, percent = percent)
      },
      error = function(e) NULL
    )
  })

  names(output_list) <- questions

  return(output_list)
}
