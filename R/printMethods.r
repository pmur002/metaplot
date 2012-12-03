##--------------metabinDF-----------------##
print.metabinDF <- function(x, ...) {
   DF <- x$DF
   colNames <- colnames(DF)
   studyNames <- as.character(DF$study)
   numStudies <- NROW(DF)
   hetero <- x$hetero
   print(list(title = x$title, subtitle = x$subtitle, colNames = colNames,
   studyNames = studyNames, numStudies = numStudies, hetero = hetero))
}
##--------------metaPar-----------------##
print.metaPar <- function(x, ...) {
  print(unlist(x))
}     