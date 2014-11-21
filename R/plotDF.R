###============plotDF generating function===============###
plotDF <- function(df, hetero, hgap, overallSum, metaClass)
{
  if (!overallSum) {
    ## step 1: generate the plotting parameters for the main DF (normal DF)
    mainDF <- df$DF
    plot.main <- data.frame(mean = mainDF["mean"],
                            lower = mainDF["lower"],
                            upper = mainDF["upper"])

    ## step 2: gap by default (normal DF)
    if (!any(metaClass %in% "groupedMetaDF")) {
      if (is.null(hgap)) {
        plot.main[nrow(plot.main) + 1, ] <- rep(NA, ncol(plot.main))
      }
    }

    ## step 3: generating the plotting parameters for the summary (normal DF)
    summary <- rbind(df$summaryFixed, df$summaryRandom)

  } else {
    ## step 1: generate plotting parameters for the summary (overall summary)
    summary <- rbind(fixed = df$overallFixed, random = df$overallRandom)
  }

  plot.sum <- data.frame(mean = summary["mean"],
                         lower = summary["lower"],
                         upper = summary["upper"])

  ## step 5: generating the plotting parameters for the hetero (normal DF)
  ## step 3: generating the plotting parameters for the hetero (overall summary)
  if (is.null(hetero)) {
    plot.hetero <- NULL
  } else {
    plot.hetero <- data.frame(mean = rep(NA, nrow(hetero)),
                              lower = rep(NA, nrow(hetero)),
                              upper = rep(NA, nrow(hetero)))
  }


  if (!overallSum) {
    ## step 6: combination (normal DF)
    if (any(metaClass %in% "groupedMetaDF")) {
      if (is.null(hgap)){
        gap <- rep(NA, NCOL(plot.main))
      } else {
        gap <- NULL
      }
      plot.DF <- rbind(plot.main, plot.sum, plot.hetero,
                       gap)
    } else {
      plot.DF <- rbind(plot.main, plot.sum, plot.hetero)
    }
  } else {
    ## step 4: combination (overall summary)
    plot.DF <- rbind(plot.sum, plot.hetero)
  }

  if (!overallSum) {
    ## step 7: set up is.summary for formatting (normal DF)
    if (any(metaClass %in% "groupedMetaDF")) {
      is.summary <- c(rep(FALSE, nrow(plot.main)),
                      rep(TRUE, nrow(summary)),
                      rep(FALSE,
                          ifelse(is.null(plot.hetero), 0, nrow(plot.hetero)) +
                          ifelse(is.null(hgap), 1, 0)))
    } else {
      is.summary <- c(rep(FALSE, nrow(plot.main)),
                      rep(TRUE, nrow(summary)),
                      rep(FALSE,
                          ifelse(is.null(plot.hetero), 0, nrow(plot.hetero))))
    }
  } else {
    ## step 5: set up is.summary for formatting (overall summary)
    is.summary <- c(rep(TRUE, nrow(summary)),
                    rep(FALSE,
                        ifelse(is.null(plot.hetero), 0, nrow(plot.hetero))))
  }

  list(plot.DF = plot.DF, is.summary = is.summary)
}
