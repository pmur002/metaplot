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
