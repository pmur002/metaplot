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
