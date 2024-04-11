#' Write formatted tables to a list for selected variables
#'
#' Write formatted tables to a list for all variable/question names contained in a
#' questions list. A question list for all questions in the current survey can be
#' made  with the `T_hss_create_question_list` function. This function silently skips
#' any questions that cause errors during creation of the contingency table,
#' labeling or formatting, and returns a list item of length NULL instead. This
#' is useful to automatically omit any question variables that have been removed
#' during data cleaning, but might in some cases lead to unexpected behavior. The
#' list of flextables created with this function can be used as input for the
#' `T_hss_export_formatted` function to write all tables to a .docx file.
#'
#' @param df The dataframe containing relevant variables
#' @param questions A named character vector containing variable names and the table type required.
#' @param group The desired grouping/disaggregation variable.
#' @param percent TRUE/FALSE if the table should be created with percentage values.
#' If set to FALSE, counts are shown.
#' @param digits The number of (significant) digits to display for percentages.
#' Set to 1 by default.
#' @param lang The language to use for labelling of questions and response options.
#' Set to "en" for english by default. Can be set to "ar" for Arabic.
#'
#' @return A named list of flextable objects containing formatted contingency tables
#' for the specified questions from the HSS.
#' @seealso `T_hss_export_formatted`, `T_hss_create_question_list`
#' @export
#'
T_hss_write_formatted <- function(df, questions, group, dict, percent = TRUE, digits = 1, lang = "en") {
  out <- lapply(questions, function(questions_element) {
    tryCatch(
      if (names(questions[match(questions_element, questions)]) == "select_one") {
        T_hss_table_single(df, questions_element, group, percent, digits) %>%
          #T_hss_label(questions_element, group, dict, lang) %>%
          T_hss_format_single() %>%
          flextable::add_footer_lines(
            values = T_hss_chisq_formatted(df, questions_element, group)) %>%
              flextable::add_footer_lines(
              values = .selection_note_single(df, lang)
              )

      } else if (names(questions[match(questions_element, questions)]) == "select_multiple") {
        T_hss_table_multi(df, questions_element, group, percent, digits) %>%
          #T_hss_label(questions_element, group, dict, lang) %>%
          T_hss_format_multi() %>%
          flextable::add_footer_lines(
            values = .selection_note_multi(df, lang)
          )
      },
      error = function(e) NULL
    )
  })
  # set names to appropriate question labels
  names(out) <- sapply(
    questions,
    function(questions_element) {
      .get_dict_varname(
        questions_element,
        "r_name",
        paste0("r_table_label_", lang),
        dict
      )
    }
  )

  return(out)
}
