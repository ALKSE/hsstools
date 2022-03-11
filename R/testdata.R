#' Generates a dataframe for testing purposes.
#'
#' @param rows An integer to set the number of rows to generate. Default is 500.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' df <- testdata(100)
testdata <- function(rows = 500) {
a <- seq_along(1:rows)
b <- factor(sample(1:2, rows, replace = TRUE))
levels(b) <- c("Gender 1", "Gender 2")
c <- factor(sample(1:5, rows, replace = TRUE))
levels(c) <- c("Location 1", "Location 2", "Location 3", "Location 4", "Location 5")
d <- factor(sample(1:5, rows, replace = TRUE))
levels(d) <- c("Answer 1", "Answer 2", "Answer 3", "Answer 4", "Answer 5")
e <- factor(sample(1:2, rows, replace = TRUE))
levels(e) <- c("Yes", "No")
f <- sample(0:1, rows , replace = TRUE)
g <- sample(0:1, rows , replace = TRUE)
h <- sample(0:1, rows , replace = TRUE)
df <- data.frame(a, b, c, d, e, f, g, h)
names(df) <- c("index", "gender", "location",
                     "singleresponse", "multiresponse",
                     "multi_option1", "multi_option2", "multi_option3")
df[df$multiresponse == "No", 6:8] <- NA
df
}
