metaDF2Matrix <- function(df, ...) {
  UseMethod("metaDF2Matrix")
}

###====================metabin=====================###
metaDF2Matrix.metabinDF <-
  function(df,
           newCols =
           list(ci = makeCIDesc("e.lower", "e.upper", 2, c("[", "]"))),
           order = c("study", "effect"),
           roundCols = c("effect" = 2, "w.fixed" = 1, "w.random" = 1),
           colNames = NULL,
           hgap = c(2, NROW(DF) + 1),
           vgap = NULL,
           stat = list(makeLabelDesc(c("I2", "tau2", "p"),
                       label = "Heterogeneity:")),
           ...)
{
  DF <- df$DF
  summary <- rbind(df$summary.fixed, df$summary.random)

  ## add new columns
  newcolnames <- names(newCols)
  for (i in newcolnames) {
    if(inherits(newCols[[i]], "coldesc")) {
      DF[, i] <- makeCol(newCols[[i]], DF)
      summary[, i] <- makeCol(newCols[[i]], summary)
    } else {
      DF[, i] <- newCols[[i]]
      summary[, i] <- ""
    }
  }

  ## round columns
  roundnames <- names(roundCols)
  for (i in roundnames) {
    DF[, i] <- sprintf(paste("%0.", roundCols[i], "f", sep = ""), DF[, i])
    summary[, i] <- sprintf(paste("%0.", roundCols[i], "f", sep = ""),
                            summary[, i])
  }

  ## rbind summary rows to DF
  DF <- rbind(DF, summary)

  ## check order argument
  if (!missing(order)) {
    if (!all(order %in% colnames(DF)))
        stop("unexpected name listed under 'order' argument")
  }

  ## create a seperate dataframe of plotting instructions
  plotDF <- data.frame(mean = DF["mean"],
                       lower = DF["lower"],
                       upper = DF["upper"])

  ## create is.summary for formatting
  is.summary <- c(rep(FALSE, NROW(df$DF)), rep(TRUE, 2))

  ##======= convert ordered data frame to matrix =======##
  matrix <- as.matrix(DF[, order])

  ## add column names
  if (missing(colNames)){
    matrix <- rbind(colnames = colnames(matrix), matrix)
  } else {
    matrix <- rbind(colnames = colNames, matrix)
  }
  is.summary <- c(TRUE, is.summary)
  plotDF <- rbind(NA, plotDF)

  nc <- NCOL(matrix)
  nr <- NROW(matrix)
  ## add heterogeneity information
  statLabels <- metabinStat(df$hetero)
  if(!is.null(stat)) {
    for (i in 1:length(stat)) {
      if(inherits(stat[[i]], "statdesc")) {
        hetero <- makeRowText(stat[[i]], df$hetero)
      } else {
        if(inherits(stat[[i]], "labeldesc")){
          hetero <- makeRowText(stat[[i]], statLabels)
        } else {
          hetero <- makeRowText(text = stat[[i]])
        }
      }
      hetero <- c(hetero, NA, rep("", nc - 2))
      matrix <- rbind(matrix, hetero)
      is.summary <- c(is.summary, FALSE)
      plotDF <- rbind(plotDF, NA)
    }
  }
  if (!is.null(df$subtitle)) {
    subtitle <- c(df$subtitle, rep(NA, nc - 1))
    matrix <- rbind(subtitle, matrix)
    is.summary <- c(TRUE, is.summary)
    plotDF <- rbind(NA, plotDF)
  }
  if (!is.null(df$title)) {
    title <- c(df$title, rep(NA, nc - 1))
    matrix <- rbind(title, matrix)
    is.summary <- c(TRUE, is.summary)
    plotDF <- rbind(NA, plotDF)
  }

  ##  insert gaps
  gap <- rep("", NCOL(matrix))
  for (i in 1:length(hgap)) {
    matrix <- rbind(matrix[1:(hgap[i] - 1), , drop=FALSE], gap,
                    matrix[hgap[i]:NROW(matrix), , drop = FALSE])
    is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE,
                    is.summary[hgap[i]:length(is.summary)])
    plotDF <- rbind(plotDF[1:(hgap[i] - 1), ], NA,
                    plotDF[hgap[i]:NROW(plotDF), ])
  }
  if (!missing(vgap)) {
    gap <- rep("     ", NROW(matrix))
    for (i in 1:length(vgap)) {
      matrix <- cbind(matrix[, 1:(vgap[i] - 1), drop = FALSE], gap,
                      matrix[, vgap[i]:NCOL(matrix), drop = FALSE])
    }
  }

  ## combine plotting information
  plotDF <- cbind(plotDF, Is.summary = is.summary)

  ## create list
  output <- list(Matrix = matrix, PlotDF = plotDF )
  class(output) <- c("metabinM", "metaM")
  output
}

###====================metacont=====================###
metaDF2Matrix.metacontDF <-
    function(df,
             newCols =
             list(ci = makeCIDesc("lower", "upper", 2, c("[", "]")),
                  msd.e = makeMSDDesc("mean.e", "sd.e", c(1, 2), c(" (", ")")),
                  msd.c = makeMSDDesc("mean.c", "sd.c", c(1, 2), c(" (", ")"))),
             order = c("study", "effect"),
             roundCols = c("effect" = 2, "w.fixed" = 1, "w.random" = 1),
             colNames = NULL, groupLab = NULL,
             hgap = c(2, NROW(DF) + 1),
             vgap = NULL, ...)
{
  ##========================Part 1=========================##
  Matrix <- NULL
  PlotDF <- NULL
  Is.summary <- c()

  if (class(df)[1] == "groupedMetaDF") {
    k <- length(df$Group) + 1
  } else {
    k <- 1 }
  ## set counter
  g <- 1
  ## set loop
  for (i in 1 : k) {
    if (k == 1) {   # no group
      data <- df
      DF <- data$DF
      summary <- rbind(data$summary.fixed, data$summary.random)
    } else {
      if (g == k) {     # group overall summary
        data <- df
        DF <- rbind(data$overall.fixed, data$overall.random)
        summary <- NULL
      } else {                     # individual group
        data <- df$Group[[i]]
        DF <- data$DF
        summary <- rbind(data$summary.fixed, data$summary.random)
      }
    }
    ## add new columns
    newcolnames <- names(newCols)
    for (i in newcolnames) {
      if(class(newCols[[i]]) == "coldesc") {
        if (is.null(summary)) {
          ## add new col to overall summary
          DF[, i] <- makeCol(newCols[[i]], DF, isSummary = TRUE)
        } else {
          ## add new col to df
          DF[, i] <- makeCol(newCols[[i]], DF)
          ## add new col to summary rows
          summary[, i] <- makeCol(newCols[[i]], summary, isSummary = TRUE)
        }
      } else {
        DF[, i] <- newCols[[i]]
        if (!is.null(summary))
            summary[, i] <- ""
      }
    }
    ## round columns
    roundnames <- names(roundCols)
    for (i in roundnames) {
      DF[, i] <- sprintf(paste("%0.", roundCols[i], "f", sep = ""), DF[, i])
      if (!is.null(summary)){
          summary[, i] <- sprintf(paste("%0.", roundCols[i], "f", sep = ""),
                                  summary[, i])
        }
    }
    ## rbind summary rows to DF
    DF <- rbind(DF, summary)
    ## check 'order' argument
    if (!missing(order)) {
      if (!all(order %in% colnames(DF)))
          stop("unexpected name listed under 'order' argument")
    }
    ##======convert ordered data frame to matrix======##
    matrix <- as.matrix(DF[, order])
    ## create is.summary for formatting
    is.summary <- c(rep(FALSE, NROW(data$DF)), rep(TRUE, 2))
    ## create a seperate dataframe of plotting instructions
    plotDF <- data.frame(mean = DF["mean"],
                         lower = DF["lower"],
                         upper = DF["upper"])
    ## add hetero and label
    nc <- NCOL(matrix)
    nr <- NROW(matrix)
    hetero <- paste("Heterogeneity: Chi-square = ",
                    round(data$hetero[1], 2),
                    " (p = ", round(data$hetero[3], 3),
                    ")", sep = "")
    hetero <- c(hetero, NA, rep("", nc - 2))
    matrix <- rbind(matrix, hetero)
    if (!missing(groupLab)) {
      label <- c(groupLab[g], NA, rep("", nc - 2))
      matrix <- rbind(label, matrix)
      is.summary <- c(TRUE, is.summary, FALSE)
      plotDF <- rbind(NA, plotDF, NA)
    } else {
      is.summary <- c(is.summary, FALSE)
      plotDF <- rbind(plotDF, NA)
    }
    ## bind groups
    Matrix <- rbind(Matrix, matrix)
    PlotDF <- rbind(PlotDF, plotDF)
    Is.summary <- c(Is.summary, is.summary)
    g <- g + 1
  }

  ##===========================Part 2=============================##
  ## add column names
  if (missing(colNames)) {
    Matrix <- rbind(colnames = colnames(Matrix), Matrix)
  } else {
    Matrix <- rbind(colnames = colNames, Matrix)
  }
  Is.summary <- c(TRUE, Is.summary)
  PlotDF <- rbind(NA, PlotDF)
  ## add titles
  if (!is.null(df$subtitle)) {
    subtitle <- c(df$subtitle, rep(NA, nc - 1))
    Matrix <- rbind(subtitle, Matrix)
    Is.summary <- c(TRUE, Is.summary)
    PlotDF <- rbind(NA, PlotDF)
  }
  if (!is.null(df$title)) {
    title <- c(df$title, rep(NA, nc - 1))
    Matrix <- rbind(title, Matrix)
    Is.summary <- c(TRUE, Is.summary)
    PlotDF <- rbind(NA, PlotDF)
  }
  ##  insert gaps
  gap <- rep("", NCOL(Matrix))
  for (i in 1:length(hgap)) {
    Matrix <- rbind(Matrix[1:(hgap[i] - 1), , drop = FALSE], gap,
                    Matrix[hgap[i]:NROW(Matrix), , drop = FALSE])
    Is.summary <- c(Is.summary[1:(hgap[i] - 1)], FALSE,
                    Is.summary[hgap[i]:length(Is.summary)])
    PlotDF <- rbind(PlotDF[1:(hgap[i] - 1),], NA,
                    PlotDF[hgap[i]:NROW(PlotDF),])
  }
  if (!missing(vgap)) {
    gap <- rep("     ", NROW(Matrix))
    Matrix <- cbind(Matrix[, 1:(vgap - 1), drop = FALSE], gap,
                    Matrix[, vgap:NCOL(Matrix), drop = FALSE])
  }
  ## combine plotting information
  PlotDF <- cbind(PlotDF, Is.summary)

  ## create list
  output <- list(Matrix = Matrix, PlotDF = PlotDF )
  class(output) <- c("metacontM", "metaM")
  output
}
