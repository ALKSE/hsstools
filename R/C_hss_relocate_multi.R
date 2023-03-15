#' Relocate split_multiple variables to expected location
#'
#' BASIC: Step two in the C_hss_split_multi pipleline. Variables are reordered
#' based on output provided by C_hss_split_multi, and relocated to their
#' expected location. Current determination on expected location relies on
#' previous experience with KOBO (where select_multiple variables were split),
#' and the reasonable expectation that sub-elements would come after their
#' _all parent.
#' PREP: Run C_hss_split_multi
#' NEXT: ...
#'
#' @param dat data-frame with all variables (not ordered)
#' @param output_1 data-frame list produced by C_hss_split_multi
#'
#' @return A data-frame that can be further cleaned
#' @export
#'
#' #' @rdname C_hss_relocate_multi
C_hss_relocate_multi <- function(dat, output_1){

  var_names <- as.list(names(output_1))
  var_names_2 <- lapply(var_names, FUN = function(i)
    x <- as.list(names(output_1[[as.character(i)]])))
  names(var_names_2) <- var_names

  z <- as.list(1:length(output_1))
  for(i in z){
    data_total_2 <- dat %>% relocate(as.character(var_names_2[[i]]),
                                     .after = as.character(names(var_names_2[i])))}
  return(data_total_2)
}


