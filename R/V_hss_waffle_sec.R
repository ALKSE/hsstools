#' Reports & Publications: Waffle visualization of security
#'
#' @param data dataframe used for visualization
#' @param x number of waffle squares
#'
#' @return a file containing the visualization
#' @export
#'
#' @rdname V_hss_waffle_sec
R_hss_waffle_sec <- function(data, x){

  sec <- data$security_change
  sec_2 <- as.data.frame(summary(sec))
  sec_3 <- as.data.frame(sec_2[1:4,])
  names(sec_3)[1] <-  "quant"
  sum <- sum(sec_3$quant)
  sec_3$percent <- (sec_3$quant/sum)*100
  sec_3$percent <- round(sec_3$percent)
  sec_4 <- sec_3$percent
  names <- rownames(sec_2)[1:4]
  sec_4 <- setNames(sec_4, names)
  print(sec_4)
  file  <- waffle::waffle(sec_4, rows = x, size = 1,
                          colors = c("#E64F1A", "#f7beab", "#ee8e6b", "#a6b0af")
  ) + theme(rect = element_rect(fill = "transparent"))

  waffle::waffle(sec_4, rows = x, size = 1,
                 colors = c("#E64F1A", "#f7beab", "#ee8e6b", "#a6b0af")
  ) + theme(rect = element_rect(fill = "transparent"))

  ggsave(file,
         filename = "ggp_transparent1.png",
         bg = "transparent")
}
