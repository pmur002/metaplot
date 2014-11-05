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
                              xlog=TRUE,
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
                               xlog=FALSE,
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
