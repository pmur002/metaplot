\name{drawMeta.metacontM}
\alias{drawMeta.metacontM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Function for 'metacontM' Objects
}
\description{
Funtion to create forest plots for objects of class \code{'metabcontM'}. 
}
\usage{
\method{drawMeta}{metacontM}(matrix, plotCol = NCOL(matrix$Matrix) + 1, 
        plotHead = "", xlab = NULL, 
        refLine = 0, plotWidth = unit(0.3, "npc"), plotPar = metaPar(), 
        xlog = FALSE, xticks = NULL, boxSize = NULL, align = NULL, 
        clip = c(-Inf, Inf), newpage=TRUE, fit = TRUE, abbreviate = FALSE,
        vpName = "Forest", ...)
}

\arguments{
  \item{matrix}{An object of class \code{'metacontM'}.}
  \item{plotCol}{Numeric column the confidence interval graph goes into.}
  \item{plotHead}{Heading for the confidence interval graph.}
  \item{xlab}{Vector of length 2 specifying direction of effect as x-axis 
                     labels.}
  \item{refLine}{x-axis coordinate for no effect line.}
  \item{plotWidth}{Width of confidence interval graph.}
  \item{plotPar}{Parameters for confidence interval graph, see 
                \code{\link{metaPar}}.}
  \item{xlog}{If \code{TRUE}, x-axis tick marks are exponentiated.}
  \item{xticks}{Optional user-specified x-axis tick marks. Specify \code{NULL}
                to use the defaults, \code{numeric(0)} to omit the x-axis.}
  \item{boxSize}{Override the default box size based on precision.}
  \item{align}{Vector giving alignment \code{("l","r","c")} for columns of table.}
  \item{clip}{Lower and upper limits for clipping confidence intervals to arrows.}
  \item{newpage}{Draw plot on a new page and overwrites current device.}
  \item{fit}{Fit plot into current viewport.}
  \item{abbreviate}{Abbreviate names of graphical objects.}
  \item{vpName}{Name of the forest plot viewport.}
  \item{\dots}{Not used.}
}
\details{
This function is more flexible but contains less arguments than the 
\code{forest.meta} method for \code{'metacont'} objects. It requires the user 
to first produce a dataframe using \code{\link{meta2DF.metacont}} and convert 
that into a text matrix using \code{\link{metaDF2Matrix.metacontDF}}. 
This process can be done with minimal input from the user, such that only 
the meta-analysis object is needed to produce the plot. 

If more flexibility is required, customisations to the plot can be made at the 
different stages of the process. Changing the order of studies by rows and 
adding extra columns from external sources can be done using
\code{\link{meta2DF.metacont}}. Reordering of columns and making new columns from
existing columns in the data frame can be done using  
\code{\link{metaDF2Matrix.metacontDF}}. Changing the position of the confidence
interval graph relative to the other text columns can be done in 
\code{\link{drawMeta.metacontM}}.
}
\value{
None
}                                           
\seealso{
\code{\link{meta2DF.metacont}},
\code{\link{metaDF2Matrix.metacontDF}}
}
\examples{
## metacont: single group     
# simple example
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD")

Data <- meta2DF(meta1) 
matrix <- metaDF2Matrix(Data, 
                        order = c("study", "effect"),
                        roundCols = c("effect" = 2),
                        hgap = 2)  
drawMeta(matrix, 
         plotCol = 3,
         plotHead = "Weighted Mean Difference")  
         
# illustrative example
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD")

Data <- meta2DF(meta1, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE) 
matrix <- metaDF2Matrix(Data, 
                        order = c("study", "msd.e", "msd.c", "effect", 
                                  "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1, 
                                       "w.random" = 1),
                        hgap = c(2, 11), vgap = 5)  
drawMeta(matrix, 
         plotCol = 6,
         plotHead = "Weighted Mean Difference")                  
## metacont: grouped studies
library(meta)
data(Fleiss93cont)
meta <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD", byvar=c(1,2,1,1,2), bylab="group") 
Data <- meta2DF(meta, title = "Mental Health Treatment",
                       rowOrder = "effect", decreasing = TRUE) 
matrix <- metaDF2Matrix(Data, groupLab = c("Group One", "Group Two", "Overall"), 
                        order = c("study", "msd.e", "msd.c", "effect",
                                        "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1, 
                                           "w.random" = 1), 
                        hgap=c(2,11,18), vgap = 5)
drawMeta(matrix, 
         plotCol = 6,
         plotHead = "Weighted Mean Difference")    
}

