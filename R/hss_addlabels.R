#' Use a list of variable names to label their associated values
#' The first half of this function uses hss_create_dic to produce 2 outputs
#' The first output is a DF with question labels, numerics, and r_names
#' The second output is a list which will be used to match var_names in the dataset to those in the DF
#' The result of this function is a new *labelled dataset
#' @param data actual survey dataset
#' @param survey the master xls form
#' @rdname hss_addlabels

hss_addlabels <- function(survey, data){
    dic_val_new <- valf(survey, "val")
    workfile <- subset(dic_val_new, select = c(list_name, r_name, label_english, name))
    workfile$final_name <- paste(workfile$list_name, workfile$r_name)
    workfile$final_name <- str_replace_all(workfile$final_name, "NA", "")
    workfile$final_name <- gsub(" ", "", workfile$final_name, fixed = TRUE)
    form <- read_excel(survey)
    new_labels <- subset(form, select = c(type, name, R_name))
    new_labels <- na.omit(new_labels)
    new_labels$type <- str_replace_all(new_labels$type, "select_one", "")
    new_labels$type <- str_replace_all(new_labels$type, "select_multiple", "")
    new_labels$type <- gsub(" ", "", new_labels$type, fixed = TRUE)
    names(new_labels)[1] <- "final_name"
    joined <<- inner_join(workfile, new_labels, by = "final_name")
    labels_for <- unique(joined$R_name)
    labels_for <<- as.list(labels_for)

    for (i in labels_for) {
      x <- filter(joined, R_name == i)
      data[,i] <- factor((data[,i]),
                         levels= x$name.x,
                         labels= x$label_english)
      print("I am labelled")
      lab_data <<- data
    }}


