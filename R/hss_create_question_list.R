#' Create a list of questions from XLS form
#'
#' @param dict_path Path to the XLS form
#'
#' @return a named character vector with question type as name and the question/variable name as value.
#' @export
#'
hss_create_question_list <- function(dict_path) {


  questions <- readxl::read_excel(dict_path) %>%
    dplyr::select(type, name) %>%
    dplyr::mutate(type = stringr::str_replace(type, "\\s.+", "")) %>%
    dplyr::filter(stringr::str_starts(name, "Q"), stringr::str_starts(type, "select")) %>%
    dplyr::pull(name, type)

  return(questions)
}
