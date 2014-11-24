### set up description format for hetero information
makeStatsDesc <- function(labelNames, heading, newStatsFormat,
                          emptyHeading = FALSE) {
  if (!emptyHeading) {
    if (missing(heading) && missing(newStatsFormat)) {
      format <- paste("Heterogeneity:", paste(rep("%s", length(labelNames)),
                                              collapse = " "))
      x <- list(format = format, statsNames = labelNames)
      class(x) <- "statsDesc"
    }

    if (!missing(heading) && missing(newStatsFormat)) {
      format <- paste(heading, paste(rep("%s", length(labelNames)),
                                     collapse = " "))
      x <- list(format = format, statsNames = labelNames)
      class(x) <- "statsDesc"
    }

    if (!missing(newStatsFormat)) {
      format <- newStatsFormat
      x <- list(format = format, statsNames = labelNames)
      class(x) <- "statsDesc"
    }
    x
  } else {
    format <- paste(paste(rep(" ", 23), collapse = ""),
                    paste(rep("%s", length(labelNames)), collapse = " "))
    x <- list(format = format, statsNames = labelNames)
    class(x) <- "statsDesc"
    x
  }
}

### generate hetero info with specified format
makeStats <- function(statsDesc, label) {
  label.names <- names(label)
  if (!all(statsDesc$statsNames %in% label.names)) {
    stop("unexpected label names in makeStatsDesc()")
  }
  args <- lapply(statsDesc$statsNames, get, label)
  args <- c(list(fmt = statsDesc$format), args)
  do.call("sprintf", args)
}
