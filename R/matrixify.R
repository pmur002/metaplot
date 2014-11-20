matrixify <- function(df, order, newCols, roundCols, stats,
                      newLabel, hgap, overallSum, metaClass, ...) {
  ## matrixify for normal DF
  if (!overallSum) {
    ## main DF
    DF <- df$DF

    ## generate main DF
    main.DF <- mainGen(df = DF, order = order, newCols = newCols,
                       roundCols = roundCols, isSummary = FALSE,
                       metaClass = metaClass)

    ## transform main DF into matrix
    matrix.DF <- as.matrix(main.DF)

    ## set up gap between main DF and summary by default
    if (!any(metaClass %in% "groupedMetaDF")) {
      if (is.null(hgap)) {
        matrix.DF <- rbind(matrix.DF, gap = "")
      }
    }

    ## summary
    ## extract the fixed and the random summary
    summary <- list(fixed = df$summaryFixed, random = df$summaryRandom)
  } else { # matrixify for overall summary
    ## summary
    # extract the overall summary
    summary <- list(fixed = df$overall.fixed, random = df$overall.random)
  }

  summary <- lapply(summary, mainGen, order = order, newCols = newCols,
                    roundCols = roundCols, isSummary = TRUE,
                    metaClass = metaClass)

  round.sum <- rbind(summary$fixed, summary$random)
  rowNames <- c("fixed", "random")
  whichShows <- sapply(summary, is.null)
  rownames(round.sum) <- rowNames[!whichShows]
  matrix.sum <- as.matrix(round.sum)

  ## hetero information
  if (is.null(stats)) {
    matrix.hetero <- NULL
  } else {
    hetero <- df$hetero

    matrix.hetero <- heteroGen(hetero = hetero, df = matrix.sum,
                               stats = stats, newLabel = newLabel,
                               metaClass = metaClass, overallSum = overallSum)

    rownames(matrix.hetero) <- rep("hetero", nrow(matrix.hetero))
  }


  ## set up the main matrix
  if (!overallSum) {
    if (any(metaClass %in% "groupedMetaDF")) {
        matrix.full <- rbind(matrix.DF, matrix.sum, hetero = matrix.hetero,
                             gap = ifelse(is.null(hgap), "", NULL))
    } else {
      matrix.full <- rbind(matrix.DF, matrix.sum, hetero = matrix.hetero)
    }
  } else {
    matrix.full <- rbind(matrix.sum, hetero = matrix.hetero)
  }

  ## plotDF
  plot.list <- plotDF(df = df, hetero = matrix.hetero, hgap = hgap,
                      overallSum = overallSum,
                      metaClass = metaClass)

  list(matrix = matrix.full, plot.DF = plot.list$plot.DF,
       is.summary = plot.list$is.summary)
}
