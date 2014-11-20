###=========================metaPlot===========================###
metaPlot <- function(x, ...) {
  UseMethod("metaPlot")
}
metaPlot.default <- function(x,...) {
  drawMeta(metaDF2Matrix(meta2DF(x)), ...)
}
metaPlot.metaDF <- function(x,...) {
  drawMeta(metaDF2Matrix(x), ...)
}
metaPlot.metaM <- function(x,...) {
  drawMeta(x, ...)
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

###==============  Generates lists of text grobs =============###
generateTextGrobs <- function(labeltext, align, is.summary, plotPar,
                              studynames = NULL, colnames = NULL, scale = 1)
{
  nc <- NCOL(labeltext)
  nr <- NROW(labeltext)
  labels <- vector("list",nc)
  if (is.null(align)) {
    align <- c("l",rep("r",nc-1))
  } else {
    align <- rep(align,length = nc)
  }
  is.summary <- rep(is.summary,length = nr)
  for(j in 1:nc){
    labels[[j]] <- vector("list", nr)
    for(i in 1:nr){
      if (is.na(labeltext[i,j])) next
      x <- switch(align[j],l = 0,r = 1,c = 0.5)
      just <- switch(align[j],l = "left",r = "right",c = "center")
      if (rownames(labeltext[i,,drop = FALSE]) == "title") {
        ## set up gp for title
        titlegp <- plotPar$title
        titlegp$cex <- scale*titlegp$cex
        gp <- do.call("gpar", titlegp)
        studynames[i] <- "title"
      } else if (rownames(labeltext[i,,drop = FALSE]) == "subtitle") {
        ## set up gp for subtitle
        subtitlegp <- plotPar$subtitle
        subtitlegp$cex <- scale*subtitlegp$cex
        gp <- do.call("gpar", subtitlegp)
        studynames[i] <- "subtitle"
      } else if (rownames(labeltext[i,,drop = FALSE]) == "hetero") {
        ## set up gp for hetero statistics
        statgp <- plotPar$stat
        if (is.summary[i]) {
          statgp$fontface <- "bold"
        }
        statgp$cex <- scale*statgp$cex
        gp <- do.call("gpar", statgp)
        studynames[i] <- "hetero"
      } else {
        ## set up gp for text
        textgp <- plotPar$text
        if (is.summary[i]) {
          textgp$fontface <- "bold"
        }
        textgp$cex <- scale*textgp$cex
        gp <- do.call("gpar", textgp)
      }
      labels[[j]][[i]] <- textGrob(name = paste("text", studynames[i],
                                     colnames[j], sep = "."),
                                 labeltext[i,j], x = x, just = just,
                                 gp = gp)}
  }
  return(labels)
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
###================== plot parameter functions =================###

### metaPar() function used in drawMeta()
metaPar <- function(...) {
  settings <- list(...)
  names <- names(settings)
  ## metaPar defaults
  defaults <- metaParDefaults()
  names <- names[names %in% names(defaults)]
  newsettings <- defaults
  for (i in names) {
    subnames <- names(settings[[i]])
    newsettings[[i]][subnames] <- settings[[i]]
  }
  class(newsettings) <- "metaPar"
  newsettings
}
### changes default settings in metaPar()
parDefaults <- function() {
  originalDefaults <- list(box = list(fill = "black"),
                           diamond = list(fill = "black"),
                           lines = list(),
                           refLine = list(col = "lightgray"),
                           summaryLine = list(col = "lightgray", lty = 2),
                           heading = list(cex = 0.95),
                           label = list(cex = 0.8),
                           axis = list(cex = 0.6),
                           title = list(cex = 1.5, fontface = "bold"),
                           subtitle = list(cex = 1, fontface = "bold.italic"),
                           text = list(cex = 1),
                           stat = list(cex = 0.8, fontface = "italic"))
  defaults <- originalDefaults
  set <- function(...) {
    settings <- list(...)
    names <- names(settings)
    names <- names[names %in% names(defaults)]
    for (i in names) {
      subnames <- names(settings[[i]])
      defaults[[i]][subnames] <<- settings[[i]]
    }
    class(defaults) <- "metaPar"
    defaults
  }
  reset <- function() {
    defaults <<- originalDefaults
  }
  list(set = set, reset = reset)
}

metaParDefaultsFuns <- parDefaults()
metaParDefaults <- metaParDefaultsFuns$set
## reset default settings
resetDefaults <- metaParDefaultsFuns$reset

###################################################################
#################### metaDF2Matrix() functions ####################
###################################################################

###=========================makeText===========================###

makeStatDesc <- function(statnames, format) {
  x <- list(format = format, statnames = statnames)
  class(x) <- "statdesc"
  x
}

## makeLabelDesc <- function(labelnames, label) {
##   format <- paste(label, paste(rep("%s",length(labelnames)), collapse = " "))
##   x <- list(format = format, statnames = labelnames)
##   class(x) <- "labeldesc"
##   x
## }

### makes a string vector of heterogeneity stats complete with labels
metabinStat <- function(dfHetero) {
  hetero <- as.list(dfHetero)
  Q <- paste("Chi-square = ", round(hetero$Q, 2)," df = ",
             hetero$df, " (p = ", round(hetero$p, 4),
             ")", sep = "")
  p <- paste("p = ", round(hetero$p, 4), sep = "")
  tau2 <- paste("tau-squared = ", round(hetero$tau2, 4), sep = "")
  H <- paste("H = ", round(hetero$H, 2), sep = "")
  H.conf <- paste("H = ", round(hetero$H, 2), "[", round(hetero$H.lower, 2),
                  ",", round(hetero$H.upper, 2), "]", sep = "")
  I2 <- paste("I-squared = ", round(100*hetero$I2, 1), "%", sep = "")
  I2.conf <- paste("I-squared = ", round(100*hetero$I2, 1), "% [",
                   round(100*hetero$I2.lower, 1), "%, ",
                   round(100*hetero$I2.upper, 1), "%]", sep = "")
  Q.CMH <- paste("Test for overall effect:", "Q = ",
                 round(hetero$Q.CMH, 2), sep = "")
  texts <- c(Q = Q, p = p, tau2 = tau2, H = H, H.conf = H.conf,
             I2 = I2, I2.conf  =  I2.conf, Q.CMH = Q.CMH)
  texts
}

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
## makeColDesc <- function(format, colnames) {
##   x <- list(format = format, colnames = colnames)
##   class(x) <- "coldesc"
##   x
## }
makeCIDesc <- function(col1 = "lower", col2 = "upper", round = 2,
                       brackets = c("[", "]"), sep = ", ") {
  makeColDesc(format = paste(brackets[1], "% .", round, "f", sep,
                  "% .", round, "f",  brackets[2], sep = ""),
              colnames = c(col1, col2))
}
makeMSDDesc <- function(col1 = "mean", col2 = "sd", round = c(1, 2),
                        brackets = c("(", ")")) {
  makeColDesc(format = paste("% .", round[1], "f", brackets[1],
                  "%.", round[2], "f",  brackets[2], sep = ""),
              colnames = c(col1, col2))
}
## makeCol <- function(coldesc, df, isSummary = FALSE) {
##   args <- lapply(coldesc$colnames, get, df)
##   ## if both columns have NA's that makes up the cell then gives blank
##   if (isSummary && any(apply(sapply(args,is.na),1,all))) {
##     return("")
##   } else {
##     args <- c(list(fmt = coldesc$format), args)
##     do.call("sprintf", args)
##   }
## }
###################################################################
######################### testing functions #######################
###################################################################

###================ add summary rows function ================###
addSummary <- function(matrix, plotDF, labels, rows) {
  ## assign names to elements in matrix
  Matrix <- matrix$Matrix
  PlotDF <- matrix$PlotDF
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
  matrix$Matrix <- Matrix
  matrix$PlotDF <- PlotDF
  matrix
}
