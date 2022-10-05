#' Use a list of variable names to label their associated values
#'
#'
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


