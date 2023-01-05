#' Use a cumulative list of variable names to label their associated values
#'
#' The first half of this function uses hss_create_dic to produce 2 outputs
#' The first output is a DF with question labels, numeric, and r_names
#' The second output is a list which will be used to match var_names in the data set
#' to those in the DF
#' The result of this function is a new *labelled data set
#' @param data actual survey data set
#' @param dict A dictionary object created with hss_create_dict()
#' @rdname hss_addlabels
#' @export

hss_addlabels <- function(data, dict){

  workfile <- subset(dic$val, select = c(list_name, r_name, label_english, name))
  workfile$final_name <- paste(workfile$list_name, workfile$r_name)
  workfile$final_name <- stringr::str_replace_all(workfile$final_name, "NA", "")
  workfile$final_name <- gsub(" ", "", workfile$final_name, fixed = TRUE)

  new_labels <- subset(dic$var, select = c(type, name, r_name))
  new_labels <- na.omit(new_labels)

  names(new_labels)[1] <- "final_name"
  joined <- dplyr::inner_join(workfile, new_labels, by = "final_name")
  joined2 <- subset(joined, select = r_name.y)
  joined2 <- unique(joined2)
  dat <- matrix(nrow = 2, ncol = nrow(joined2))
  dat <- as.data.frame(dat)

  colnames(dat) <- joined2$r_name.y
  matches <- janitor::compare_df_cols(data, dat, return = "mismatch")
  labels_for <- unique(matches$column_name)
  labels_for <- as.list(labels_for)

  for (i in labels_for) {
    x <- dplyr::filter(joined, r_name.y == i)
    data[[i]] <- factor(data[[i]],
                        levels= x$name.x,
                        labels= x$label_english)
  }
  return(data)
}



