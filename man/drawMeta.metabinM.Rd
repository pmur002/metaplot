\name{drawMeta.metabinM}
\alias{drawMeta.metabinM}
\title{
Plot Function for 'metabinM' Objects
}
\description{
Function to create forest plots for objects of class \code{'metabinM'}. 
}
\usage{
\method{drawMeta}{metabinM}(matrix, plotCol = NCOL(matrix$Matrix) + 1, 
        plotHead = "", xlab = NULL, 
        refLine = 0, plotWidth = unit(0.3, "npc"), plotPar = metaPar(), 
        xlog = TRUE, xticks = NULL, boxSize = NULL, align = NULL, 
        clip = log(c(0.05,6)), newpage=TRUE, fit = TRUE, abbreviate = FALSE, 
        vpName = "Forest", ...)
}

\arguments{
  \item{matrix}{An object of class \code{'metabinM'}.}
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
\code{forest.meta} method for \code{metabin} objects in the \pkg{meta} package. 
It requires the user to first produce a dataframe using 
\code{\link{meta2DF.metabin}} and convert that into a
text matrix using \code{\link{metaDF2Matrix.metabinDF}}. This process can be
done with minimal input from the user, such that only the meta-analysis
object is needed to produce the plot. 

If more flexibility is required, customisations to the plot can be made at the 
different stages of the process. Changing the order of studies by rows and 
adding extra columns from external sources can be done using
\code{\link{meta2DF.metabin}}. Reordering of columns and making new columns from
existing columns in the data frame can be done using  
\code{\link{metaDF2Matrix.metabinDF}}. Changing the position of the confidence
interval graph relative to the other text columns can be done in 
\code{\link{drawMeta.metabinM}}.
}
\value{
None
}
\seealso{
\code{\link{meta2DF.metabin}},
\code{\link{metaDF2Matrix.metabinDF}}
}
\examples{
##simple example
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")                                
Data <- meta2DF(meta1)                 
matrix <- metaDF2Matrix(Data,  
                        order = c("study", "effect", "w.random"),     
                        roundCols = c("effect" = 2, "w.random" = 1),
                        hgap = c(2, 7))                          
drawMeta(matrix,
         plotCol = 1,
         plotHead = "Relative risk (log scale)")
         
#confidence interval plot customisations
drawMeta(matrix,
         plotCol = 2,
         clip=log(c(0.05,6)),
         refLine = log(2),
         boxSize = 0.75,
         plotWidth = unit(2,"inches"), 
         plotHead = "Relative risk (log scale)")
         
##illustrative example
# testing 'add' argument
add <- list(test1 = c(1:4), test2 = c(5:8))          
Data <- meta2DF(meta1, title = "Thrombolytic Therapy" , 
                    rowOrder = "effect", decreasing = TRUE,
                    add = add)               
matrix <- metaDF2Matrix(Data,  
                         order = c("study", "event.e", "event.c", "effect", 
                                  "ci", "w.fixed", "w.random"),     
                         roundCols = c("effect" = 2, "w.fixed" = 1, 
                                        "w.random" = 1),
                         hgap = c(2, 8, 11), 
                         newCols=list(ci=makeCIDesc("e.lower", "e.upper", 2, 
                         c("[", "]")))
                         )
drawMeta(matrix, 
         plotCol = 5,
         clip=log(c(0.05,6)),                   
         xlab = c("Favours treatment","Favours control"),
         plotHead = "Relative risk (log scale)", 
         plotPar = metaPar(box=list(fill="royalblue", col="royalblue"),
                           lines=list(col="darkblue"),
                           diamond=list(fill="royalblue", col="royalblue"))) 
## Customising graphical objects using grid.edit()
# grid.ls()
## grid.edit("text.Thrombolytic.Therapy.study", gp=gpar(cex=1.5, col="blue"))
grid.edit("[.]X51[.]", global=TRUE, grep=TRUE, gp=gpar(col="red"))                          
}