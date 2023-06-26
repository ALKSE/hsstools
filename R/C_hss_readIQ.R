#' Read Encoded Iraq (Arabic) Data
#'
#' This function cleans up the procedure for reading-in Iraq (Arabic) data.
#' Encoding is a key faccet of this function. Arabic letters are not
#' legibile without this function.
#'
#' @return Raw data with proper encoding for Arabic text
#' @export
#'
#' @rdname C_hss_readIQ
H_hss_readIQ <- function(){
  Folder <- "Raw Data"
  file_list <- list.files(path=Folder, pattern="*.csv")
  IQ_dat <- read.csv(paste(Folder, file_list[1], sep='/'),
                     encoding="UTF-8", stringsAsFactors=FALSE)}
