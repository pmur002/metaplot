setGroupLabel <- function(matrix, groupLab) {
  groupLabel <- c(groupLab, rep(NA, ncol(matrix$matrix) - 1))
  matrix$matrix <- rbind(groupLabel, matrix$matrix)
  matrix$plot.DF <- rbind(NA, matrix$plot.DF)
  matrix$is.summary <- c(TRUE, matrix$is.summary)
  matrix
}
