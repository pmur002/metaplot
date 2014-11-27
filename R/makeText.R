### makes a string vector of heterogeneity stats complete with labels
## metabinStat <- function(dfHetero) {
##   hetero <- as.list(dfHetero)
##   Q <- paste("Chi-square = ", round(hetero$Q, 2)," df = ",
##              hetero$df, " (p = ", round(hetero$p, 4),
##              ")", sep = "")
##   p <- paste("p = ", round(hetero$p, 4), sep = "")
##   tau2 <- paste("tau-squared = ", round(hetero$tau2, 4), sep = "")
##   H <- paste("H = ", round(hetero$H, 2), sep = "")
##   H.conf <- paste("H = ", round(hetero$H, 2), "[", round(hetero$H.lower, 2),
##                   ",", round(hetero$H.upper, 2), "]", sep = "")
##   I2 <- paste("I-squared = ", round(100*hetero$I2, 1), "%", sep = "")
##   I2.conf <- paste("I-squared = ", round(100*hetero$I2, 1), "% [",
##                    round(100*hetero$I2.lower, 1), "%, ",
##                    round(100*hetero$I2.upper, 1), "%]", sep = "")
##   Q.CMH <- paste("Test for overall effect:", "Q = ",
##                  round(hetero$Q.CMH, 2), sep = "")
##   texts <- c(Q = Q, p = p, tau2 = tau2, H = H, H.conf = H.conf,
##              I2 = I2, I2.conf  =  I2.conf, Q.CMH = Q.CMH)
##   texts
## }

makeRowText <- function(statdesc = NULL, hetero = NULL, text = NULL) {
  if(!is.null(statdesc)) {
    hetero <- as.list(hetero)
    args <- lapply(statdesc$statnames, get, hetero)
    ## putting together arguments
    args <- c(list(fmt = statdesc$format), args)
  } else {
    ## print out text in the argument
    args <- list(text)
  }
  ## call sprintf
  do.call("sprintf", args)
}

###================== new column functions ===================###
makeCIDesc <- function(col1 = "lower", col2 = "upper", round = 2,
                       brackets = c("[", "]"), sep = ", ") {
  makeColDesc(format = paste(brackets[1], "% .", round, "f", sep,
                  "% .", round, "f",  brackets[2], sep = ""),
              colNames = c(col1, col2))
}
makeMSDDesc <- function(col1 = "mean", col2 = "sd", round = c(1, 2),
                        brackets = c("(", ")")) {
  makeColDesc(format = paste("% .", round[1], "f", brackets[1],
                  "%.", round[2], "f",  brackets[2], sep = ""),
              colNames = c(col1, col2))
}

###================ add summary rows function ================###
addSummary <- function(matrix, plotDF, labels, rows) {
  ## assign names to elements in matrix
  Matrix <- matrix$matrix
  PlotDF <- matrix$plotDF
  ## create a list of row labels corresponding to the columns of the Matrix
  addRow <- list()
  for (i in 1:length(labels)) {
    addRow[[i]] <- rep("", NCOL(Matrix))
    names(addRow[[i]]) <- colnames(Matrix)
    for (j in names(labels[[i]])) {
      addRow[[i]][j] <- labels[[i]][j]
    }
  }
  ## rbind addRow or an empty row to the Matrix and PlotDF
  k <- 1
  for (i in (NROW(Matrix) + 1):(max(rows) + 1)) {
    if (i == rows[k] && k <= length(rows)) {
      Matrix <- rbind(Matrix, addRow[[k]])
      PlotDF <- rbind(PlotDF, plotDF[k, ])
      k <- k + 1
    } else {
      Matrix <- rbind(Matrix, rep("", NCOL(Matrix)))
      PlotDF <- rbind(PlotDF, NA)
      PlotDF[i,"Is.summary"] <- FALSE
    }
  }
  matrix$matrix <- Matrix
  matrix$plotDF <- PlotDF
  matrix
}
