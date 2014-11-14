\name{metaPlot}
\alias{metaPlot}

\title{
Forest Plot Function 
}
\description{
A generic convenience function that draws forest plots with default
settings. 
}
\usage{
metaPlot(x, ...)
}

\arguments{
  \item{x}{An object of class \code{'metabin'},
    \code{'metacont'}, \code{'metaDF'}, or \code{'metaM'}.}
  \item{\dots}{Additional arguments in \code{\link{drawMeta}}.}
}
\details{

  This function provides a convenient way to draw forest plots in only
  one step. It assumes default values for intermediate functions
  \code{meta2DF} and \code{metaDF2Matrix} if input is a meta-analysis
  object.

If \code{x} is a meta-analysis object, calling \code{metaPlot(x)} is
same as doing the following steps:

\code{
DF <- meta2DF(x), 
Matrix <- metaDF2Matrix(DF), 
drawMeta(Matrix)}
}
\value{
None 
}
%% \seealso{
%% \code{\link{metaPlot.default}}
%% \code{\link{metaPlot.metaDF}}
%% \code{\link{metaPlot.metaM}}
%% }
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data = Olkin95, 
                 subset = c(41,47,51,59), sm = "RR", method = "I")
Data <- meta2DF(meta1)     
matrix <- metaDF2Matrix(Data)                          
### input metabin object  
metaPlot(meta1)
### input metaDF object 
metaPlot(Data)
### input metaM object 
metaPlot(matrix)
### additional drawMeta() arguments 
metaPlot(meta1, plotCol = 2, plotHead = "Relative Risk") 
} 