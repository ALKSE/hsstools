#' Create a list of questions from XLS form
#'
#' @param dict_path Path to the XLS form
#'
#' @return a named character vector with question type as name and the question/variable name as value.
#' @export
#'
hss_create_question_list <- function(dict_path) {
  questions <- read_excel(dict_path) %>%
    select(type, name) %>%
    mutate(type = str_replace(type, "\\s.+", "")) %>%
    filter(str_starts(name, "Q"), str_starts(type, "select")) %>%
    pull(name, type)

  return(questions)
}
