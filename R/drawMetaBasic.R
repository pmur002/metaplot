drawMetaBasic <- function(matrix, plotCol, plotHead, xlab, refLine,
                          plotWidth, plotPar, xlog, xticks, boxSize,
                          align, clip, newpage, fit,
                          abbreviate, vpName, ...)
{
  require("rmeta") || stop("'rmeta' package not found")

  ## assigning names to "matrix" components
  labeltext <- matrix$Matrix
  mean <- matrix$PlotDF[, "mean"]
  lower <- matrix$PlotDF[, "lower"]
  upper <- matrix$PlotDF[, "upper"]
  is.summary <- matrix$PlotDF[, "Is.summary"]

  ## creating vectors of study(row) and column names to label grobs
  if (abbreviate) {
    studynames <- make.names(abbreviate(labeltext[, "study"], 6))
    colnames <- make.names(abbreviate(colnames(labeltext), 6))
  } else {
    studynames <- make.names(labeltext[, "study"])
    colnames <- make.names(colnames(labeltext))
  }
  ## calculate number of rows and columns
  nc <- NCOL(labeltext)
  nr <- NROW(labeltext)
  ## set height of each row as unit 'lines'
  ## where cex of texts is the number of lines per row
  lineScale <- plotPar$text$cex
  if (rownames(labeltext[1, , drop = FALSE]) == "title") {
    rowHeights <- unit(c(plotPar$title$cex, rep(lineScale, nr - 1), 0.5),
                       "lines")
  } else {
    rowHeights <- unit(c(rep(lineScale, nr), 0.5), "lines")
  }
  ## see if any row has NA
  rowWidth <- !apply(is.na(labeltext), 1, any)
  ## start new page
  if (newpage) grid.newpage()
  ## work out plot scale
  if (fit) {
    scale <- fitPlot(labeltext, align, is.summary, plotPar,
                     plotWidth, plotCol, rowWidth, rowHeights)
  } else {
    scale <- 1
  }
  ## generate labels and width/height caluclations with scaling
  labels <- generateTextGrobs(labeltext, align, is.summary, plotPar,
                              studynames = studynames, colnames = colnames,
                              scale = scale)
  colWidth <- columnWidths(labels, nc, rowWidth, colgap = 3*scale,
                            plotCol, plotWidth)
  rowHeights <- scale*rowHeights
  ## push viewports
  pushViewport(viewport(layout = grid.layout(nr + 1, nc*2 + 2,
                                             widths = colWidth,
                                             heights = rowHeights),
                        name = vpName))
  cwidth <- (upper - lower)
  xrange <- c(max(min(lower, na.rm = TRUE), clip[1]),
              min(max(upper, na.rm = TRUE), clip[2]))
  info <- 1/cwidth
  info <- info/max(info[!is.summary], na.rm = TRUE)
  info[is.summary] <- 1
  if (!is.null(boxSize)) {
    info <- rep(boxSize, length = length(info))
  }
  ## push viewports with layout to draw texts
  for(j in 1:nc){
    for(i in 1:nr){
      if (!is.null(labels[[j]][[i]])){
        if (j < plotCol){
          pushViewport(viewport(layout.pos.row = i,
                                layout.pos.col = 2*j - 1,
                                name = paste(labels[[j]][[i]]$name, i, j,
                                             sep = "."))
                       )
        } else {
          pushViewport(viewport(layout.pos.row = i,
                                layout.pos.col = 2*(j + 1) - 1,
                                name = paste(labels[[j]][[i]]$name, i, j,
                                             sep = "."))
                       )
        }
        grid.draw(labels[[j]][[i]])
        upViewport()
      }
    }
  }
  ## push viewport for plotting
  pushViewport(viewport(layout.pos.col = plotCol*2 - 1, xscale = xrange,
                        gp = gpar(cex = scale), name = "Graph"))
  ## draw no effect line
  grid.lines(name = "refLine",
             x = unit(refLine,"native"),
             y = unit(c(0, (nr - 2)*lineScale), "lines"),
             gp = do.call("gpar", plotPar$refLine))
  ## draw overall effects line
  for (i in 1:nr) {
    if (is.summary[i]){
      grid.lines(name= paste("summaryLine", studynames[i], sep = "."),
                 x = unit(mean[i], "native"),
                 y = unit(c(0, (nr - 2)*lineScale), "lines"),
                 gp = do.call("gpar", plotPar$summaryLine))
    }
  }
  ## draw x-axis
  if (xlog){
    if(is.null(xticks)){
      ticks <- pretty(exp(xrange))
      if (clip[1] == -Inf) {
        ## add 0.5 and 1 to axis label
        ticks <- unique(sort(c(0.5, 1, ticks)))
      }
      ticks <- ticks[ticks > 0]
    } else {
      ticks <- xticks
    }
    if (length(ticks)){
      if (min(lower, na.rm = TRUE) < clip[1]) {
        ticks <- c(exp(clip[1]), ticks)
      }
      if (max(upper, na.rm = TRUE) > clip[2]){
        ticks <- c(ticks, exp(clip[2]))
      }
      xax <- xaxisGrob(gp = do.call("gpar", plotPar$axis),
                       at = log(ticks), name = "xax")
      xax1 <- editGrob(xax, gPath("labels"),
                       ## sprintf get rid of trailing zeros in label
                       label = sprintf("%g",
                                       as.numeric(format(ticks, digits = 2))))
         grid.draw(xax1)
     }
  } else {
    if (is.null(xticks)){
      grid.xaxis(name = "xax",
                 gp = do.call("gpar", plotPar$axis))
    } else if(length(xticks)) {
      grid.xaxis(name = "xax",at = xticks,
                 gp = do.call("gpar", plotPar$axis))
    }
  }
  ## draw plot heading
  grid.text(name = "plotHead", plotHead,
            y = unit((nr - 0.5)*lineScale, "lines"),
            gp = do.call("gpar", plotPar$heading))
  ## draw axis labels for effect tendency
  if(!is.null(xlab)) {
    plotPar$label$cex <- plotPar$label$cex
    grid.text(name = "xlab1", xlab[1],
              x = unit(refLine - 0.3, "native"),
              y = unit(lineScale, "lines"),
              just = "right", gp = do.call("gpar", plotPar$label))
    grid.text(name = "xlab2", xlab[2],
              x = unit(refLine + 0.3, "native"),
              y = unit(lineScale, "lines"),
              just = "left", gp = do.call("gpar", plotPar$label))
  }
  upViewport()
  ## draw confidence interval lines and polygons
  for (i in 1:nr) {
    if (is.na(mean[i])) next
    ## 1
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = plotCol*2 - 1,
                          xscale = xrange, gp = gpar(cex = scale),
                          name = paste("CI", studynames[i], i, j, sep = ".")))
    if (is.summary[i]){
      drawSummaryCI(lower[i], mean[i], upper[i], info[i],
                    studynames[i], plotPar)
    } else {
      drawNormalCI(lower[i], mean[i], upper[i], info[i],
                   studynames[i], plotPar)
    }
    upViewport()
  }
  upViewport()
}
