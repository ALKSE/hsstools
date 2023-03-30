#' Update your dictionary of HSS variable or value labels
#'
#' This update is intended for better presentation of Iraq(subdistrict) data.
#' Note: Area refers to district in the XLS form, and in real terms to
#' subdistirct distribution.
#'
#' @param dict Dictionary object.
#' @param area The survey location for which the specific XLS form was made.
#' @return A list containing two dataframes, with respectively variable- and value
#' names and their associated text labels *with added area filter.
#' @export
#'
#' @rdname C_hss_update_dict

C_hss_update_dict <- function(dict, area) {
  area <- tolower(area)
  dict$val <- dplyr::filter(
    dict$val,
    .data$area == .env$area | is.na(.data$area))
  return(dict)
}

