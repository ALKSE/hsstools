#' Relocate split_multiple variables to expected location
#'
#'
#' @param dat data-frame with all variables (not ordered)
#' @param output_multi data-frame list produced by C_hss_split_multi
#' @param var_names_2 naming list to assist in relocation process
#'
#' @return A data-frame that can be further cleaned
#' @export
#'
#' @rdname C_hss_relocate_multi
  C_hss_relocate_multi <- function(data, var_names_2, output_multi){
    #relocation function underlying loop
    loco <- function(data, num){
      a <- as.character(var_names_2[[as.numeric(num)]])
      b <- names(var_names_2[as.numeric(num)])
      dat <- data %>% dplyr::relocate(as.character(a),
                                      .after = as.character(b))
      return(dat)}

    #actual looping function
    output <- data
    for(i in seq_along(output_multi)){
      dt <- loco(output, i)
      output <- dt}
    return(output)
  }



