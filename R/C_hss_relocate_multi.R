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
  C_hss_relocate_multi <- function(data, dict, var_names_2, output_multi){

    #preperatory material to guide proper placement of _multi vars
    merged <- dplyr::right_join(dict$var %>% dplyr::select(type, name, r_name),
                                dict$val %>% dplyr::select(list_name, name, r_name),
                                by = c("type" = "list_name")
    ) %>% dplyr::filter(!is.na(r_name.y))


    merged$col_name <- paste(merged$r_name.x, merged$r_name.y, sep = "") %>%
      gsub("_all_", "_", .) %>%
      unique()

    #relocation function underlying loop
    loco <- function(data, num){
      a <- names(var_names_2[as.numeric(num)])
      b <- subset(merged, r_name.x == a)
      c <- as.character(b$col_name)

      dat <- data %>% dplyr::relocate(as.character(c),
                                      .after = as.character(a))
      return(dat)}

    #actual looping function
    output <- data
    for(i in seq_along(output_multi)){
      dt <- loco(output, i)
      output <- dt}
    return(output)
  }



