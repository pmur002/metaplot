dropRows <- function(metaMatrix, drop) {
  if (is.numeric(drop)) {
    if (max(drop) > nrow(metaMatrix$matrix))
      stop("the number of rows dropped is larger than the number of rows in matrix")
    metaMatrix$matrix <- metaMatrix$matrix[-drop, ]
    metaMatrix$plotDF <- metaMatrix$plotDF[-drop, ]
  } else {
    if (!all(drop %in% rownames(metaMatrix$matrix)))
      stop("the dropped rows do not exist")
    drop.rows <- which(rownames(metaMatrix$matrix) %in% drop)
    metaMatrix$matrix <- metaMatrix$matrix[-drop.rows, ]
    metaMatrix$plotDF <- metaMatrix$plotDF[-drop.rows, ]
  }
  metaMatrix
}
