#' Apply HSS formatting to multi-response contingecy table
#'
#' @param x A flextable object.
#'
#' @return A flextable object.
#' @export
#'
#' @examples
#' WIP
hssformat_multi <- function(x) {
  # header
  x <- bg(x, bg = "#E64F1A", part = "header")
  x <- color(x, color = "white", part = "header")
  x <- add_header_row(x, values = "question", colwidth = ncol_keys(x))
  # body
  x <- bg(x, bg = "#F2F2F2", part = "body")
  x <- bg(x, i = ~ p <= 0.05, bg = "#D9D9D9", part = "body")
  x <- bold(x, i = ~p <= 0.05, part = "body")
  x <- colformat_num(x, suffix = "%")
  x <- colformat_num(x, j = "p", suffix = "")
  # all
  x <- void(x, j = "p", part = "all")
  x <- autofit(x, part = c("header", "body"))
  x <- hline(x, border = fp_border(color = "black", style = "solid", width = 1), part = "all")
  x <- hline_top(x, border = fp_border(color = "black", style = "solid", width = 1), part = "all")
  return(x)
}
