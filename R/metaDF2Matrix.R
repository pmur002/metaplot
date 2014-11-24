###========================metaDF2Matrix========================###
metaDF2Matrix <- function(df, ...) {
  UseMethod("metaDF2Matrix")
}

###=========================metacontDF============================###
metaDF2Matrix.metacontDF <-
    function(df, order, newCols = NULL,
             roundCols = NULL,
             stats = list(hetero = makeStatsDesc(labelNames =
                                                 c("Q", "p", "df"))),
             groupStats = NULL, newLabel = NULL, colNames,
             groupLab, hgap = NULL, vgap, drop, ...)
{
  if (!inherits(df, "groupedMetaDF")){
    ## default setting for order
    if (missing(order)) {
      order <- c("study", "effect")
    }

    ## generate matrix for plotting
    metaMatrix <- matrixify(df = df, order = order, newCols = newCols,
                            roundCols = roundCols, stats = stats,
                            newLabel = newLabel, hgap = hgap,
                            overallSum = FALSE,
                            metaClass = class(df), ...)

    plot.matrix <- metaMatrix$matrix
    plot.DF <- metaMatrix$plot.DF
    is.summary <- metaMatrix$is.summary



    ## set up column names
    if (missing(colNames)) {
      plot.matrix <- rbind(colnames = colnames(plot.matrix), plot.matrix)
    } else {
      if (length(colNames) != ncol(plot.matrix))
        stop("the number of column names does not match the number of columns")
      plot.matrix <- rbind(colnames = colNames, plot.matrix)
    }
    ## plot.DF
    plot.DF <- rbind(NA, plot.DF)
    is.summary <- c(TRUE, is.summary)

    ## set up title and subtitle
    if (!is.null(df$subtitle)) {
      subtitle <- c(df$subtitle, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap)) {
        plot.matrix <- rbind(subtitle = subtitle, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(subtitle = subtitle, plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    if (!is.null(df$title)) {
      title <- c(df$title, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap) && is.null(df$subtitle)) {
        plot.matrix <- rbind(title = title, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(title = title,  plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    ## insert gaps by users specification
    if (!is.null(hgap)) {
      for (i in 1:length(hgap)) {
        plot.matrix <- rbind(plot.matrix[1:(hgap[i] - 1), , drop = FALSE],
                             gap = "",
                             plot.matrix[hgap[i]:nrow(plot.matrix), ,
                                         drop = FALSE])
        plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                         plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
        is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE,
                        is.summary[hgap[i]:length(is.summary)])
      }
    }
    if (!missing(vgap)) {
      for (i in 1:length(vgap)) {
        plot.matrix <- cbind(plot.matrix[, 1:(vgap[i] - 1), drop = FALSE],
                             gap = "",
                             plot.matrix[, vgap[i]:ncol(plot.matrix),
                                         drop = FALSE])
      }
    }
    plot.DF <- cbind(plot.DF, is.summary)
    output <- list(matrix = plot.matrix, plotDF = plot.DF)

    ## drop rows by users requirements
    if (!missing(drop)) {
      output <- dropRows(metaMatrix = output, drop = drop)
    }

    class(output) <- c("metacontM", "metaM")
    output
  } else {
    ## default setting for order
    if (missing(order)) {
      order <- c("study", "effect")
    }

    ## generate matrix for plotting
    groupDF <- df$Group
    groupMetaMatrix <- lapply(groupDF, matrixify, order = order,
                              newCols = newCols,
                              roundCols = roundCols, stats = stats,
                              newLabel = newLabel,
                              hgap = hgap, overallSum = FALSE,
                              metaClass = class(df))

    if (is.null(groupStats)) {
      groupStats <- stats
    }

    overall.sum <- matrixify(df = df, order = order, newCols = newCols,
                             roundCols = roundCols, stats = groupStats,
                             newLabel = newLabel,
                             hgap = hgap, overallSum = TRUE,
                             metaClass = class(df))

    groupMetaMatrix[[length(groupMetaMatrix) + 1]] <- overall.sum


    ## set up group label for sub-group
    if (!missing(groupLab)) {
      if (!(length(groupLab) == length(groupMetaMatrix)))
          stop("number of groups does not match the number of group labels")
      groupMatrix <-  mapply(setGroupLabel, groupMetaMatrix, groupLab)
      group.plot.matrix <- groupMatrix[1, ]
      group.plot.DF <- groupMatrix[2, ]
      group.is.summary <- groupMatrix[3, ]
    } else {
      group.plot.matrix <- lapply(groupMetaMatrix, get, x = "matrix")
      group.plot.DF <- lapply(groupMetaMatrix, get, x = "plot.DF")
      group.is.summary <- lapply(groupMetaMatrix, get, x = "is.summary")
    }

    ## combine groups
    plot.matrix <- NULL
    plot.DF <- NULL
    is.summary <- NULL
    for (i in 1:length(groupMetaMatrix)) {
      plot.matrix <- rbind(plot.matrix, group.plot.matrix[[i]])
      plot.DF <- rbind(plot.DF, group.plot.DF[[i]])
      is.summary <- c(is.summary, group.is.summary[[i]])
    }

    ## set up column names
    if (missing(colNames)) {
      plot.matrix <- rbind(colnames = colnames(plot.matrix), plot.matrix)
    } else {
      if (length(colNames) != ncol(plot.matrix))
        stop("the number of column names does not match the number of columns")
      plot.matrix <- rbind(colnames = colNames, plot.matrix)
    }
    ## plot.DF
    plot.DF <- rbind(NA, plot.DF)
    is.summary <- c(TRUE, is.summary)

    ## set up title and subtitle
    if (!is.null(df$subtitle)) {
      subtitle <- c(df$subtitle, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap)) {
        plot.matrix <- rbind(subtitle = subtitle, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(subtitle = subtitle, plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    if (!is.null(df$title)) {
      title <- c(df$title, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap) && is.null(df$subtitle)) {
        plot.matrix <- rbind(title = title, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(title = title,  plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    ## insert gaps by users specification
    if (!is.null(hgap)) {
      for (i in 1:length(hgap)) {
        plot.matrix <- rbind(plot.matrix[1:(hgap[i] - 1), , drop = FALSE],
                             gap = "",
                             plot.matrix[hgap[i]:nrow(plot.matrix), ,
                                         drop = FALSE])
        plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                         plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
        is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE,
                        is.summary[hgap[i]:length(is.summary)])
      }
    }
    if (!missing(vgap)) {
      for (i in 1:length(vgap)) {
        plot.matrix <- cbind(plot.matrix[, 1:(vgap[i] - 1), drop = FALSE],
                             gap = "",
                             plot.matrix[, vgap[i]:ncol(plot.matrix),
                                         drop = FALSE])
      }
    }
    plot.DF <- cbind(plot.DF, is.summary)
    output <- list(matrix = plot.matrix, plotDF = plot.DF)

    ## drop rows by users requirements
    if (!missing(drop)) {
      output <- dropRows(metaMatrix = output, drop = drop)
    }

    class(output) <- c("metacontM", "metaM")
    output
  }
}

###=========================metabinDF============================###
metaDF2Matrix.metabinDF <-
    function(df, order, newCols = NULL, roundCols = NULL,
             stats = list(hetero = makeStatsDesc(labelNames =
                                                 c("I2", "tau2", "p"))),
             groupStats = NULL, newLabel = NULL,
             colNames, groupLab, hgap = NULL, vgap, drop, ...)
{
  if (!inherits(df, "groupedMetaDF")){
    ## default setting for order
    if (missing(order)) {
      order <- c("study", "effect")
    }

    ## generate matrix for plotting
    metaMatrix <- matrixify(df = df, order = order, newCols = newCols,
                            roundCols = roundCols, stats = stats,
                            newLabel = newLabel, hgap = hgap,
                            overallSum = FALSE,
                            metaClass = class(df), ...)

    plot.matrix <- metaMatrix$matrix
    plot.DF <- metaMatrix$plot.DF
    is.summary <- metaMatrix$is.summary

    ## set up column names
    if (missing(colNames)) {
      plot.matrix <- rbind(colnames = colnames(plot.matrix), plot.matrix)
    } else {
      if (length(colNames) != ncol(plot.matrix))
        stop("the number of column names does not match the number of columns")
      plot.matrix <- rbind(colnames = colNames, plot.matrix)
    }
    ## plot.DF
    plot.DF <- rbind(NA, plot.DF)
    is.summary <- c(TRUE, is.summary)

    ## set up title and subtitle
    if (!is.null(df$subtitle)) {
      subtitle <- c(df$subtitle, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap)) {
        plot.matrix <- rbind(subtitle = subtitle, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(subtitle = subtitle, plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    if (!is.null(df$title)) {
      title <- c(df$title, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap) && is.null(df$subtitle)) {
        plot.matrix <- rbind(title = title, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(title = title,  plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    ## insert gaps by users specification
    if (!is.null(hgap)) {
      for (i in 1:length(hgap)) {
        plot.matrix <- rbind(plot.matrix[1:(hgap[i] - 1), , drop = FALSE],
                             gap = "",
                             plot.matrix[hgap[i]:nrow(plot.matrix), ,
                                         drop = FALSE])
        plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                         plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
        is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE,
                        is.summary[hgap[i]:length(is.summary)])
      }
    }
    if (!missing(vgap)) {
      for (i in 1:length(vgap)) {
        plot.matrix <- cbind(plot.matrix[, 1:(vgap[i] - 1), drop = FALSE],
                             gap = "",
                             plot.matrix[, vgap[i]:ncol(plot.matrix),
                                         drop = FALSE])
      }
    }
    plot.DF <- cbind(plot.DF, is.summary)
    output <- list(matrix = plot.matrix, plotDF = plot.DF)

    ## drop rows by users requirements
    if (!missing(drop)) {
      output <- dropRows(metaMatrix = output, drop = drop)
    }

    class(output) <- c("metabinM", "metaM")
    output
  } else {
    ## default setting for order
    if (missing(order)) {
      order <- c("study", "effect")
    }

    ## generate matrix for plotting
    groupDF <- df$Group
    groupMetaMatrix <- lapply(groupDF, matrixify, order = order,
                              newCols = newCols,
                              roundCols = roundCols, stats = stats,
                              newLabel = newLabel,
                              hgap = hgap, overallSum = FALSE,
                              metaClass = class(df))

    if (is.null(groupStats)) {
      groupStats <- stats
    }

    overall.sum <- matrixify(df = df, order = order, newCols = newCols,
                             roundCols = roundCols, stats = groupStats,
                             newLabel = newLabel,
                             hgap = hgap, overallSum = TRUE,
                             metaClass = class(df))

    groupMetaMatrix[[length(groupMetaMatrix) + 1]] <- overall.sum


    ## set up group label for sub-group
    if (!missing(groupLab)) {
      if (!(length(groupLab) == length(groupMetaMatrix)))
          stop("number of groups does not match the number of group labels")
      groupMatrix <-  mapply(setGroupLabel, groupMetaMatrix, groupLab)
      group.plot.matrix <- groupMatrix[1, ]
      group.plot.DF <- groupMatrix[2, ]
      group.is.summary <- groupMatrix[3, ]
    } else {
      group.plot.matrix <- lapply(groupMetaMatrix, get, x = "matrix")
      group.plot.DF <- lapply(groupMetaMatrix, get, x = "plot.DF")
      group.is.summary <- lapply(groupMetaMatrix, get, x = "is.summary")
    }

    ## combine groups
    plot.matrix <- NULL
    plot.DF <- NULL
    is.summary <- NULL
    for (i in 1:length(groupMetaMatrix)) {
      plot.matrix <- rbind(plot.matrix, group.plot.matrix[[i]])
      plot.DF <- rbind(plot.DF, group.plot.DF[[i]])
      is.summary <- c(is.summary, group.is.summary[[i]])
    }

    ## set up column names
    if (missing(colNames)) {
      plot.matrix <- rbind(colnames = colnames(plot.matrix), plot.matrix)
    }else {
      if (length(colNames) != ncol(plot.matrix))
        stop("the number of column names does not match the number of columns")
      plot.matrix <- rbind(colnames = colNames, plot.matrix)
    }
    ## plot.DF
    plot.DF <- rbind(NA, plot.DF)
    is.summary <- c(TRUE, is.summary)

    ## set up title and subtitle
    if (!is.null(df$subtitle)) {
      subtitle <- c(df$subtitle, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap)) {
        plot.matrix <- rbind(subtitle = subtitle, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(subtitle = subtitle, plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    if (!is.null(df$title)) {
      title <- c(df$title, rep(NA, ncol(plot.matrix) - 1))
      if (is.null(hgap) && is.null(df$subtitle)) {
        plot.matrix <- rbind(title = title, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      } else {
        plot.matrix <- rbind(title = title,  plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)
      }
    }

    ## insert gaps by users specification
    if (!is.null(hgap)) {
      for (i in 1:length(hgap)) {
        plot.matrix <- rbind(plot.matrix[1:(hgap[i] - 1), , drop = FALSE],
                             gap = "",
                             plot.matrix[hgap[i]:nrow(plot.matrix), ,
                                         drop = FALSE])
        plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                         plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
        is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE,
                        is.summary[hgap[i]:length(is.summary)])
      }
    }
    if (!missing(vgap)) {
      for (i in 1:length(vgap)) {
        plot.matrix <- cbind(plot.matrix[, 1:(vgap[i] - 1), drop = FALSE],
                             gap = "",
                             plot.matrix[, vgap[i]:ncol(plot.matrix),
                                         drop = FALSE])
      }
    }
    plot.DF <- cbind(plot.DF, is.summary)
    output <- list(matrix = plot.matrix, plotDF = plot.DF)

    ## drop rows by users requirements
    if (!missing(drop)) {
      output <- dropRows(metaMatrix = output, drop = drop)
    }

    class(output) <- c("metabinM", "metaM")
    output
  }
}
