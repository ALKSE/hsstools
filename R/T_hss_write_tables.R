#' Write tables to a list for selected variables
#'
#' Write (unformatted) tables to a list for all variable/question names contained in a
#' questions list. A question list for all questions in the current survey can be
#' made  with the `hss_create_question_list` function. This function silently skips
#' any questions that cause errors during creation of the contingency table, and
#' returns a list item of length NULL instead. This is useful to automatically omit
#' any question variables that have been removed during data cleaning, but might in
#' some cases lead to unexpected behavior. The list of dataframes created with this
#' function can be used as input for `hss_export_tables` to write the tables to a
#' .csv file.
#'
#' @param df The dataframe containing relevant variables
#' @param questions A named character vector containing variable names and the table type required.
#' @param group The desired grouping/disaggregation variable.
#' @param percent Logical vector if the table should be created with percentage values. If set to FALSE, counts are shown.
#'
#' @return A named list of dataframes containing contingecy tables for the specified
#' questions of the HSS.
#' @seealso `hss_export_tables`, `hss_create_question_list`
#' @export
#'
T_hss_write_tables <- function(df, questions, group, percent = TRUE, digits = 3) {
  output_list <- lapply(questions, function(questions_element) {
    tryCatch(

      if (names(questions[match(questions_element, questions)]) == "select_one") {
        table <- list(
          table = T_hss_table_single(df, questions_element, group, percent = percent, digits = digits),
          p = T_hss_chisq_formatted(df, questions_element, group)
        )
      } else if (names(questions[match(questions_element, questions)]) == "select_multiple") {
        table <- T_hss_table_multi(df, questions_element, group, percent = percent, digits = digits)
      },
      error = function(e) NULL
    )
  })

  names(output_list) <- questions

  return(output_list)
}
