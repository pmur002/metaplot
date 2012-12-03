\name{drawMeta}
\alias{drawMeta}

\title{
Plot Function
}
\description{
A generic function that can be used to draw forest plots. 
}
\usage{
drawMeta(matrix, ...)
}

\arguments{
  \item{matrix}{Either an object of class \code{'metabinM'}, or an object of class 
                \code{'metacontM'}. See 'Details'.}
  \item{\dots}{Other arguments.}
}
\details{
Currently, object \code{x} can come from functions 
\code{\link{metaDF2Matrix.metabinDF}} 
or \code{\link{metaDF2Matrix.metacontDF}}.
}
\value{
None
}
\note{
The code for \code{drawMeta()} generic function is based on Thomas Lumley's 
\code{forestplot()} function in the \pkg{rmeta} package.
} 
\seealso{
\code{\link{drawMeta.metabinM}},
\code{\link{drawMeta.metacontM}}
}
\examples{
##simple example
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")                                
Data <- meta2DF(meta1)                 
matrix <- metaDF2Matrix(Data)                          
drawMeta(matrix)

#confidence interval plot customisations
drawMeta(matrix,
         plotCol = 2,
         refLine = log(2),
         boxSize = 0.75,
         plotWidth = unit(2,"inches"), 
         plothead = "Relative risk (log scale)")
                        
##illustrative example
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")
#testing 'add' argument
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
         plotCol = 6,
         clip=log(c(0.05,6)),                   
         xlab = c("Favours treatment","Favours control"),
         plotHead = "Relative risk (log scale)", 
         plotPar = metaPar(box=list(fill="royalblue", col="royalblue"),
                           lines=list(col="darkblue"),
                           diamond=list(fill="royalblue", col="royalblue"),
                           title=list(cex=2))) 

## Customising graphical objects using grid.edit()
# grid.ls()
# grid.edit("text.title.study", gp=gpar(cex=1.5, col="blue"))
grid.edit("[.]X51[.]", global=TRUE, grep=TRUE, gp=gpar(col="red"))      
downViewport("Graph")
grid.rect(gp=gpar(lty=5))
upViewport()
}
