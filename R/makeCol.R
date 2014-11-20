### set up description format for column
makeColDesc <- function(format, colNames){
  x <- list(format = format, colNames = colNames)
  class(x) <- "colDesc"
  x
}

### generate column with specified format
makeCol <- function(colDesc, df, isSummary) {
  col.names <- names(df)
  if (!all(colDesc$colNames %in% col.names)) {
    stop("unexpected column names in makeColDesc()")
  }
  args <- lapply(colDesc$colNames, get, df)
  ## check if the column cells are NA
  check.NA <- any(ifelse(is.null(nrow(sapply(args, is.na))),
                         all(sapply(args, is.na)), # with 1 row
                         apply(sapply(args, is.na), 1, all))) # multiple rows
  if (isSummary && check.NA) {
    return("")
  } else {
    args <- c(list(fmt = colDesc$format), args)
    do.call("sprintf", args)
  }
}
