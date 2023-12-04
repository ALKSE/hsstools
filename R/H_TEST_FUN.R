#' TEST FUNCTIONS
#'
#' This is a playground for functions under consideration.
#'
#' @export
#'

#Generate the sub_names of a _all variable
multi_valname <- function(var, df) {

  answers <- names(
    dplyr::select(
      df,
      dplyr::starts_with(
        stringr::str_replace(var, "_all", "_")
      ) & !dplyr::ends_with(c("_what", "_all"))
    )
  )
  return(answers)
}

#Calculate sum of a specific var/coloumn
sum_values <- function(dat, var){
  exp <- dat %>% select(dplyr::contains(as.character(var)))
  return(sum(exp))
}
