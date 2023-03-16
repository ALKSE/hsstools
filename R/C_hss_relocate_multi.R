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
#' @rdname C_hss_relocate_multi
C_hss_relocate_multi <- function(dat, output_1){
 #(1) TITLE: PREPARING LIST OF VARIABLES FOR RELOCATION
 #(1) Explanation: A list of variable names and data-frames is created to make
 # relocation more systematic (allowing for a loop).
 #(1) Output: A list of data-frames focused on 'name'
  var_names <- as.list(names(output_1))
  var_names_2 <- lapply(var_names, FUN = function(i)
    x <- as.list(names(output_1[[as.character(i)]])))
  names(var_names_2) <- var_names

  #------------------------------------------------------------------------
  #(2) TITLE: LOOPING THROUGH NAME LIST AND RELOCATING VARIABLES
  #(2) Explanation: Using a FOR loop, select_multiple variables are relocated
  # based on the list produced earlier.
  #(2) Output: An ordered data-frame that can be cleaned further.

  #(2.1) A numeric list is created for easier looping logic.
  z <- as.list(1:length(output_1))
  for(i in z){
    data_total_2 <- dat %>% dplyr::relocate(as.character(var_names_2[[i]]),
                                     .after = as.character(names(var_names_2[i])))}
  return(data_total_2)
}


