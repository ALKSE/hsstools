#' Create a dictionary of HSS variable or value labels
#'
#' Create a dictionary from an XLS form. The dictionary is stored as a dataframe,
#' and contains the contents of the XLS form stored in a (slightly modified) format
#' that allows it to be used as a dictionary for looking up variable & value labels.
#' This second argument of this function allows for filtering of questions and responses
#' that are specific to, or different between, certain locations. Please check the
#' XLS form in question for survey location abbreviations.
#'
#' @param form Path to the XLS form.
#' @param location The survey location for which the specific XLS form was made.
#' @return A list containing two dataframes, with respectively variable- and value
#' names and their associated text labels.
#' @export
#'
#' @rdname hss_create_dict
hss_create_dict <- function(form, location = NULL) {
  dict <- list(
    var = readxl::read_xls(path = form, sheet = 1) %>%
      janitor::clean_names() %>%
      tidyr::separate(
        col = type,
        into = c("q_type", "type"),
        sep = " ",
        fill = "left"
      ),
    val = readxl::read_xls(path = form, sheet = 2) %>%
      janitor::clean_names()
  )

  if (!is.null(location)) {
    location <- tolower(location)
    dict$val <- dplyr::filter(
      dict$val,
      .data$location == .env$location | is.na(.data$location)
    )
  }
  return(dict)
}
