### set up description format for label
makeLabelDesc <- function(format, heteroNames) {
  x <- list(format = format, heteroNames = heteroNames)
  class(x) <- "labelDesc"
  x
}

### generate label with specified format
makeLabel <- function(labelDesc, hetero) {
  hetero.names <- names(hetero)
  if (!all(labelDesc$heteroNames %in% hetero.names)) {
    stop("unexpected hetero names in makeLabelDesc()")
  }
  hetero <- as.list(hetero)
  args <- lapply(labelDesc$heteroNames, get, hetero)
  args <- c(list(fmt = labelDesc$format), args)
  do.call("sprintf", args)
}
