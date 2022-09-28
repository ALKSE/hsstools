hss_pload <- function() {
  lapply((load <- c("dplyr", "flextable", "forcats", "janitor", "lubridate", "magrittr", "officer", "purrr", "questionr", "stringr", "tidyr", "writexl", "haven", "Hmisc", "readr", "readxl")), require, character.only = TRUE)
}
