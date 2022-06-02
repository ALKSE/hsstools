#' Create a list of questions from XLS form
#'
#' This function reads the XLS form and provides a list of all questions in the form as well as
#' the type of question.
#'
#' @param dict_path Path to the XLS form
#'
#' @return A named character vector with question type as name and the question/variable name as value.
#' @export
#'
hss_create_question_list <- function(dict_path) {
  dict <- hss_create_dict(dict_path, "var")
  questions <- dplyr::select(dict, q_type, name, r_name) %>%
    dplyr::filter(
      stringr::str_starts(name, "Q"),
      stringr::str_starts(q_type, "select")
    ) %>%
    dplyr::pull(r_name, q_type)

  return(questions)
}
