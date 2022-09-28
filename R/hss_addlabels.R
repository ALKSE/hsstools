#' Use a list of variable names to label their associated values
#'
#' A prerequisite for this function is the existence of a variable list. This list can be created from
#' XML form. In the current iteration of this function, the list is not being created, simply used.
#' The next commit will hopefully include added parameters that generate the list.
#'
#'
#' @rdname hss_addlabels

hss_addlabels <- function(data, list){
  value_labels <- subset(dic_val, select = c(list_name, name, label_english))
  for (i in list) {
    x <- filter(value_labels, list_name == i)
    data[,i] <- factor((data[,i]),
                       levels= x$name,
                       labels= x$label_english)
  }
  data_new <<- data
  return(data_new)
}
