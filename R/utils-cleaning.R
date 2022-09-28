# Load the XLS form for renaming purposes
.load_xlsform <- function(file) {
  form <- readxl::read_xls(file) %>%
    janitor::clean_names()
  form$type <- gsub("select_one |select_multiple ", "", form$type)

  form_val <- readxl::read_xls(file, sheet = 2) %>%
    janitor::clean_names()

  out <- list(form = form, form_val = form_val)
  return(out)
}
# Create a mapping of old & new variable names based on provided XLS form. Output
# is dataframe with columns 'oldvar' and 'newvar'
.create_mapping <- function(list) {
  merged <- right_join(list$form %>% select(type, name, r_name),
                       list$form_val %>% select(list_name, name, r_name),
                       by = c("type" = "list_name")
  ) %>% filter(!is.na(r_name.y))

  oldname2 <- paste(merged$name.x, merged$name.y, sep = "_") %>% unique()

  newname2 <- paste(merged$r_name.x, merged$r_name.y, sep = "") %>%
    gsub("_all_", "_", .) %>% unique()

  oldvar <- c(list$form$name, oldname2) %>% tolower()
  newvar <- c(list$form$r_name, newname2) %>% tolower()

  out <- data.frame(oldvar, newvar) %>%
    mutate(newvar = if_else(is.na(newvar), oldvar, newvar)) %>%
    filter(!is.na(oldvar))
  return(out)
}
# Replaces old names with new names. Accepts a character string as input. If an
# element of 'names' is found in the mapping object, it is replaced with its corresponding
# new name. If not found, the original name is kept.
.apply_mapping <- function(names, mapping) {
  # just in case
  names = janitor::make_clean_names(names)

  names <- sapply(names, function(x) {
    if (length(mapping$newvar[which(mapping$oldvar == x)]) == 0) {
      x
    } else {
      mapping$newvar[which(mapping$oldvar == x)]
    }
  })
  return(names)
}
