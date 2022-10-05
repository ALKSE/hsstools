#' Merge HSS survey rounds into one dataframe
#'
#' Merges multiple survey rounds from the same location into a single dataframe.
#' The function expects stata .dta files and requires a mapping file containing
#' the changes of variable names throughout survey rounds. The resulting dataframe
#' contains only variables present in all datasets. Output is stripped of its haven
#' labels (if any).
#'
#' @param folder a folder containing the .dta files to be merged. The function does
#' not check whether files belong to the same survey location.
#' @param mapping a mapping dataframe containing the variable names for the different
#' survey rounds and their relation to each other.
#'
#' @return Returns a dataframe consisting of all columns present in each of the
#' provided datasets.
#' @export
hss_merge_datasets <- function(folder, mapping) {
  dat <- load_files(folder) %>%
    lapply(function(x) apply_newnames(x, mapping))

  vars <- lapply(dat, function(x) names(x)) %>%
    Reduce(intersect, .)

  out <- lapply(dat, function(x) dplyr::select(x, all_of(vars))) %>%
    do.call(rbind.data.frame, .)
  return(out)
}
#' @keywords internal
load_files <- function(folder) {
  #find all .dta files in the specified folder. Does not check if files are actually from same survey location
  files <- list.files(path = folder, pattern = ".dta\\b", full.names = TRUE)
  dat <- lapply(files, function(filename) {
    haven::read_dta(x) %>%
      haven::zap_labels() %>%
      haven::zap_label() %>%
      dplyr::mutate(year = stringr::str_extract(filename, "\\d{4}"))
  })
  # files from surveyround 2018 need the 'Q01' part from their variable names removed
  yearcheck <- sapply(dat, function(dataset) 2018 %in% dataset$year)
  names(dat[[which(yearcheck == TRUE)]]) <- gsub("Q\\d+\\w?_", "", names(dat[[which(yearcheck == TRUE)]]))
  return(dat)
}
#' @keywords internal
apply_newnames <- function(dat, mapping) {
  # determine most applicable survey round based on no. of matching names in each
  # column of the mapping file
  surveyround <- sapply(mapping, function(x) sum(names(dat) %in% x)) %>%
    which.max() %>%
    names()

  newnames <- sapply(names(dat), function(oldname) {
    # newname only applied if variable name is found in appropriate mapping column
    # and the corresponding value from the newest column is not empty.
    # rightmost column is expected to be the newest.
    if (oldname %in% mapping[[surveyround]]) {
      if (!is.na(mapping[[length(names(mapping))]][which(mapping[[surveyround]] == oldname)])) {
        mapping[[length(names(mapping))]][which(mapping[[surveyround]] == oldname)]
      } else {
        oldname
      }
    } else {
      oldname
    }
  }, USE.NAMES = FALSE) %>%
    unlist()

  names(dat) <- newnames
  return(dat)
}
