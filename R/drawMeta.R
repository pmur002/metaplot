drawMeta <- function(matrix, ...) {
  UseMethod("drawMeta")
}
###====================metabin=====================###
drawMeta.metabinM <- function(matrix,
                              plotCol = NCOL(matrix$Matrix) + 1,
                              plotHead = "",
                              xlab = NULL,
                              refLine = 0,
                              plotWidth = unit(0.3,"npc"),
                              plotPar = metaPar(),
                              xlog = TRUE,
                              xticks = NULL,
                              boxSize = NULL,
                              align = NULL,
                              clip = log(c(0.05,6)),
                              newpage = TRUE,
                              fit = TRUE,
                              abbreviate = FALSE,
                              vpName = "Forest", ... )
{
  drawMetaBasic(matrix,
                plotCol = plotCol,
                plotHead = plotHead,
                xlab = xlab,
                refLine = refLine,
                plotWidth = plotWidth,
                plotPar = plotPar,
                xlog = xlog,
                xticks = xticks,
                boxSize = boxSize,
                align = align,
                clip = clip,
                newpage = newpage,
                fit = fit,
                abbreviate = abbreviate,
                vpName = vpName, ...)
}

###====================metacont=====================###
drawMeta.metacontM <- function(matrix,
                               plotCol = NCOL(matrix$Matrix) + 1,
                               plotHead = "",
                               xlab = NULL,
                               refLine = 0,
                               plotWidth = unit(0.3,"npc"),
                               plotPar = metaPar(),
                               xlog = FALSE,
                               xticks = NULL,
                               boxSize = NULL,
                               align = NULL,
                               clip = c(-Inf,Inf),
                               newpage = TRUE,
                               fit = TRUE,
                               abbreviate = FALSE,
                               vpName = "Forest", ... )
{
  drawMetaBasic(matrix,
                plotCol = plotCol,
                plotHead = plotHead,
                xlab = xlab,
                refLine = refLine,
                plotWidth = plotWidth,
                plotPar = plotPar,
                xlog = xlog,
                xticks = xticks,
                boxSize = boxSize,
                align = align,
                clip = clip,
                newpage = newpage,
                fit = fit,
                abbreviate = abbreviate,
                vpName = vpName, ...)
}

###################################################################
####################### drawMeta() functions ######################
###################################################################

###=============== Draw a non-summary rect-plus-CI ===============###
drawNormalCI <- function(LL, OR, UL, size,
                         studynames, plotPar) {
  size <- 0.75*size
  clipupper <- convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 1
  cliplower <- convertX(unit(LL, "native"), "npc", valueOnly = TRUE) <0
  box <-  convertX(unit(OR, "native"), "npc", valueOnly = TRUE)
  clipbox <- box<0 || box>1
  ## Draw arrow if exceed col range
  ## convertX() used to convert between coordinate systems
  if (clipupper || cliplower) {
    ends <- "both"
    lims <- unit(c(0, 1), c("npc", "npc"))
    if (!clipupper) {
      ends <- "first"
      lims <- unit(c(0, UL), c("npc","native"))
    }
    if (!cliplower) {
      ends <- "last"
      lims <- unit(c(LL, 1), c("native", "npc"))
    }
    grid.lines(name = paste("line", studynames, sep = "."),
               x = lims, y = 0.5,
               arrow = arrow(ends = ends,length = unit(0.05, "inches")),
               gp = do.call("gpar", plotPar$lines))
    if (!clipbox) {
        grid.rect(name = paste("rect", studynames, sep = "."),
                  x = unit(OR, "native"),
                  width = unit(size, "snpc"), height = unit(size, "snpc"),
                  gp = do.call("gpar", plotPar$box))
      }
  } else   {
    ## Draw line white if totally inside rect
    grid.lines(name = paste("line", studynames, sep = "."),
               x = unit(c(LL, UL), "native"), y = 0.5,
               gp = do.call("gpar", plotPar$lines))
    grid.rect(name = paste("rect", studynames, sep = "."),
              x = unit(OR, "native"),
              width = unit(size, "snpc"), height = unit(size, "snpc"),
              gp = do.call("gpar", plotPar$box))
    if ((convertX(unit(OR, "native") + unit(0.5*size, "lines"), "native",
                  valueOnly = TRUE) > UL) &&
        (convertX(unit(OR, "native") - unit(0.5*size, "lines"), "native",
                  valueOnly = TRUE) < LL))
        grid.lines(name = paste("line", studynames, sep = "."),
                   x = unit(c(LL, UL), "native"), y = 0.5,
                   gp = do.call("gpar", plotPar$lines))
  }
}

###===============  Draw a summary "diamond" ==================###
drawSummaryCI <- function(LL, OR, UL, size, studynames, plotPar) {
  grid.polygon(name = paste("diamond", studynames, sep = "."),
               x = unit(c(LL, OR, UL, OR), "native"),
               y = unit(0.5 + c(0, 0.5*size, 0, -0.5*size), "npc"),
               gp = do.call("gpar", plotPar$diamond))
}


###================= Calculates width of labels ===============###
columnWidths <- function(labels, nc, rowWidth, colgap, plotCol, plotWidth)
{
  colgap <- unit(colgap,"mm")
  if (1 == plotCol)  {
    colWidth <- unit.c(plotWidth, colgap)
    for(i in 2:(nc+1)) {
      colWidth <- unit.c(colWidth,
                         max(unit(rep(1, sum(rowWidth)),
                                  "grobwidth", labels[[i - 1]][rowWidth])),
                         colgap)
    }
  } else {
    colWidth <- unit.c(max(unit(rep(1, sum(rowWidth)), "grobwidth",
                                labels[[1]][rowWidth])), colgap)
    for(i in 2:(nc+1)){
      colWidth <-
          if (i == plotCol) {
            unit.c(colWidth, plotWidth, colgap)
          } else {
            if (i < plotCol) {
              unit.c(colWidth, max(unit(rep(1,sum(rowWidth)), "grobwidth",
                                        labels[[i]][rowWidth])), colgap)
            } else {
              unit.c(colWidth, max(unit(rep(1,sum(rowWidth)), "grobwidth",
                                        labels[[i-1]][rowWidth])), colgap) }
          }
    }
  }
  return(colWidth)
}

###============= Calculates scale to fit plot on page =============###
fitPlot <- function(labeltext, align, is.summary, plotPar,
                    plotWidth, plotCol, rowWidth, rowHeights)
{
  nc <- NCOL(labeltext)
  nr <- NROW(labeltext)
  ## generate text grobs from labeltext
  labels <- generateTextGrobs(labeltext, align, is.summary, plotPar)
  ## calculate plot and page widths
  colWidth <- columnWidths(labels, nc, rowWidth, colgap = 3, plotCol, plotWidth)
  totalwidth <- convertWidth(sum(colWidth),"inches", valueOnly = TRUE)
  pagewidth <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
  ## include only text in width calculation
  plotvalue <-  convertWidth(plotWidth, "inches", valueOnly = TRUE)
  textwidth <- totalwidth - plotvalue
  pagetextwidth <- pagewidth - plotvalue
  wscale <- 1
  ## calculate width scale to fit current viewport #####
  while (textwidth > pagetextwidth && pagewidth > plotvalue) {
    wscale <- wscale * pagetextwidth / textwidth
    labels <- generateTextGrobs(labeltext, align, is.summary,
                                plotPar, scale = wscale)
    colWidth <- columnWidths(labels, nc, rowWidth, colgap = 3*wscale,
                             plotCol, plotWidth)
    textwidth <- convertWidth(sum(colWidth), "inches", valueOnly = TRUE) -
        plotvalue
  }
  ## calculate height scale to fit current viewport #####
  heights <- rowHeights
  totalheight <- convertHeight(sum(rowHeights),"inches", valueOnly = TRUE)
  pageheight <- convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)
  hscale <- 1
  while (totalheight > pageheight) {
    hscale <- hscale * pageheight / totalheight
    heights <- hscale * rowHeights
    totalheight <- convertHeight(sum(heights),"inches", valueOnly = TRUE)
  }
  ## use the smaller scale
  if (hscale > wscale) {
    scale <- wscale
  } else {
    scale <- hscale
  }
  scale
}

