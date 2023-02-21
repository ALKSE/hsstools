#' Use a cumulative list of variable names to label their associated values
#'
#' The first half of this function uses hss_create_dic to produce 2 outputs
#' The first output is a DF with question labels, numeric, and r_names
#' The second output is a list which will be used to match var_names in the data set
#' to those in the DF
#' The result of this function is a new *labelled data set
#' @param data actual survey data set
#' @param language The language of survey levels
#' @rdname hss_vallabel
#' @export

hss_vallabel <- function(dat, language){
  if(language == "English") {.vallabel_en(dat)} else {.vallabel_ar(dat)}}



