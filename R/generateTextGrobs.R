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
