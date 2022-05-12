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

  questions <- readxl::read_excel(dict_path) %>%
    janitor::clean_names() %>%
    dplyr::select(type, name, r_name) %>%
    dplyr::mutate(
      type = stringr::str_replace(type, "\\s.+", "")
    ) %>%
    dplyr::filter(
      stringr::str_starts(name, "Q"),
      stringr::str_starts(type, "select")
    ) %>%
    dplyr::pull(r_name, type)

  return(questions)
}
