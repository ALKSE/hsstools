#' Apply HSS formatting to contingency table
#'
#' @param x A flextable object.
#'
#' @return a formatted flextable.
#' @export
#'
#' @examples
#' WIP
hssformat <- function(x) {
  # header
  x <- bg(x, bg = "#E64F1A", part = "header")
  x <- color(x, color = "white", part = "header")
  # body
  x <- bg(x, bg = "#F2F2F2", part = "body")
  x <-set_table_properties(x, layout = "autofit")
  # footer
  x <- add_footer_row(
    x,
    values = paste(
      "Chi-squared is ",
      if (p > 0.05) "not",
      " significant (p = ",
      round(p, digits = 3),
      ")",
      sep = ""
    ),
    colwidth = ncol_keys(x)
  )
  # all
  x <- hline(x, border = fp_border(color = "black", style = "solid", width = 1), part = "all")
  x <- hline_top(x, border = fp_border(color = "black", style = "solid", width = 1), part = "all")
}
