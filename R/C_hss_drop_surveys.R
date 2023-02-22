#' Drop invalid surveys based on selection criteria
#'
#' HSS surveys are considered valid when they meet a number of selection criteria.
#' This is a combined function that allows for the selection of the survey country.
#' Additional details can be found within two other utility functions:
#' - utils-IQDrop
#' - utils-SSDrop
#'
#' @param dat The dataframe containing HSS surveys
#' @param country The HSS location; South Sudan or Iraq
#' @param audit Inclusion of an audit file for South Sudan
#'
#' @return A dataframe containing only those surveys that meet the selection criteria
#' @export
C_hss_drop_surveys <- function(dat, country, audit){
  if ((country == "Iraq") & (missing(audit))) {.drop_IQ(dat)}
  else if ((country == "South Sudan") & (missing(audit))) {.drop_SS(dat, audit = NULL)}
  else if ((country == "South Sudan") & (!!missing(audit))) {.drop_SS(dat, audit = NULL)}
}




