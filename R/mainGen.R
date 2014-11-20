###============main DF matrix generating function===================###
mainGen <- function(df, order, newCols, roundCols, isSummary, metaClass) {
  if (is.null(df)) {
    df
  } else {
    ## step 1: check arguments
    if (any(metaClass == "metacontDF")) {
      if (!all(order %in%
               c(names(df), names(newCols), "ci", "msd.e", "msd.c"))) {
        stop("unexpected column names input in the argument 'order'")
      }
    }
    if (any(metaClass == "metabinDF")) {
      if (!all(order %in% c(names(df), names(newCols), "ci"))) {
        stop("unexpected column names input in the argument 'order'")
      }
    }


    if (!is.null(newCols)){
      if (!all(sapply(newCols, inherits, what = "colDesc"))) {
        stop("unexpected class listed under 'newCols'")
      }
    }

    ## step 2: add columns with specific format into main DF
    if (any(metaClass == "metacontDF")) {
      if (any(order %in% c("ci", "msd.e", "msd.c", names(newCols)))) {
        df <- addCols(df = df, order = order, newCols = newCols,
                      isSummary = isSummary, metaClass = metaClass)
      }
    }

    if (any(metaClass == "metabinDF")) {
      if (any(order %in% c("ci", names(newCols)))) {
        df <- addCols(df = df, order = order, newCols = newCols,
                      isSummary = isSummary, metaClass = metaClass)
      }
    }


    ## step 3: extract the required columns to form a new data frame
    df <- df[order]

    ## step 4: round up the main DF
    df <- roundUpCols(df = df, newCols = newCols, roundCols = roundCols,
                      isSummary = isSummary, metaClass = metaClass)

    df
  }
}
###=============================addCols==============================###

## generate a set of columns to be used in mainGen()
addCols <- function(df, order, newCols, isSummary, metaClass) {
  default.format <- list()
  if (any(metaClass == "metacontDF")) {
    default.format$ci <- list(format = paste("[", "%.", 2, "f", ", ",
                                             "%.", 2, "f", "]", sep =""),
                              colNames = c("lower", "upper"))
    default.format$msd.e <- list(format = paste("% .", 1, "f", "(", "%.",
                                                2, "f", ")", sep = ""),
                                 colNames = c("mean.e", "sd.e"))
    default.format$msd.c <- list(format = paste("% .", 1, "f", "(", "%.",
                                                2, "f", ")", sep = ""),
                                 colNames = c("mean.c", "sd.c"))
    if (is.null(newCols)) {
      col.format <- default.format
    } else {
      col.format <- list()
      if (!any(names(newCols) %in% "ci")) {
        col.format$ci <- default.format$ci
      }
      if (!any(names(newCols) %in% "msd.e")) {
        col.format$msd.e <- default.format$msd.e
      }
      if (!any(names(newCols) %in% "msd.c")) {
        col.format$msd.c <- default.format$msd.c
      }
    }
  }

  if (any(metaClass == "metabinDF")) {
    default.format$ci <- list(format = paste("[", "%.", 2, "f", ", ",
                                             "%.", 2, "f", "]", sep =""),
                              colNames = c("e.lower", "e.upper"))
    if (is.null(newCols)) {
      col.format <- default.format
    } else {
      col.format <- list()
      if (!any(names(newCols) %in% "ci")) {
        col.format$ci <- default.format$ci
      }
    }
  }

  col.desc <- lapply(col.format, do.call, what = "makeColDesc")
  col.desc <- c(col.desc, newCols)
  if (any(metaClass == "metacontDF")) {
    req.col <- order[order %in% c("ci", "msd.e", "msd.c", names(newCols))]
  } else {
    req.col <- order[order %in% c("ci", names(newCols))]
  }
  col.desc <- col.desc[req.col]
  new.col <- lapply(col.desc, makeCol, df = df, isSummary = isSummary)
  new.col <- as.data.frame(new.col)
  cbind(df, new.col)
}

###=============================roundUpCol==================================###

### set up round for the column
setUpRound <- function(var, roundCols, isSummary, metaClass) {
  if (any(metaClass == "metacontDF")) {
    defaultRoundCols <- c(n.e = 0, mean.e = 1, n.c = 0, mean.c = 1, sd.e = 2,
                          sd.c = 2, effect = 2,
                          w.fixed = ifelse(isSummary, 0, 1),
                          w.random = ifelse(isSummary, 0, 1),
                          mean = 2, lower = 2,
                          upper = 2, other = 2)
  }
  if (any(metaClass == "metabinDF")) {
    defaultRoundCols <- c(n.e = 0, event.e = 0, n.c = 0, event.c = 0,
                          effect = 2,
                          se = 2, w.fixed = ifelse(isSummary, 0, 1),
                          w.random = ifelse(isSummary, 0, 1),
                          mean = 2, lower = 2,
                          upper = 2, e.lower = 2, e.upper = 2, other = 2)
  }

  if (is.null(roundCols)) {
    if (var %in% names(defaultRoundCols)) {
      rounding <- defaultRoundCols[var]
      as.numeric(rounding)
    } else {
      rounding <- defaultRoundCols["other"]
      as.numeric(rounding)
    }
  } else {
    if (var %in% names(defaultRoundCols)) {
      rounding <- switch(as.character(roundCols[var]),
                         "NA" = defaultRoundCols[var],
                         roundCols[var])
      as.numeric(rounding)
    } else {
      rounding <- switch(as.character(roundCols[var]),
                         "NA" = defaultRoundCols["other"],
                         roundCols[var])
      as.numeric(rounding)
    }
  }
}

### round up the column with specified requirement
roundUpCols <- function(df, newCols, roundCols, isSummary, metaClass) {
  if (all(names(df) %in% c("study", "ci", "msd.e",
                           "msd.c", names(newCols)))) {
    df
  } else {
    temp.DF <- df[!(names(df) %in% c("study", "ci", "msd.e",
                                     "msd.c", names(newCols)))]

    if (ncol(temp.DF) == 1 && !sapply(temp.DF, is.numeric)) {
      df
    } else {
      var <- names(temp.DF)
      var <- var[sapply(temp.DF, function(df) is.numeric(df) || is.na(df))]

      round.cols <- sapply(var, setUpRound, roundCols = roundCols,
                           isSummary = isSummary, metaClass = metaClass)
      round.names <- names(round.cols)
      for (i in round.names) {
        df[, i] <- sprintf(paste("%0.", round.cols[i], "f", sep = ""),
                           df[, i])
      }
      df
    }
  }
}


