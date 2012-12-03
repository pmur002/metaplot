\name{meta2DF}
\alias{meta2DF}

\title{
Convert Meta-analysis Objects to Data Frames
}
\description{
A generic function that turns meta-analysis objects into a list of data frames 
used for plotting. 
}
\usage{
meta2DF(meta, ...)
}
\arguments{
  \item{meta}{Either an object of class \code{metabin}, or an object of class 
           \code{'metacont'}. See 'Details'.}
  \item{\dots}{Other arguments.}
}
\details{
Currently, object \code{meta} can come from functions \code{metabin()} or 
\code{metacont()} from \pkg{meta} package.
}
\seealso{
\code{\link{meta2DF.metabin}},
\code{\link{meta2DF.metacont}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")
Data1 <- meta2DF(meta1)
Data1
# testing 'add' argument
add <- list(test1 = c(1:4), test2 = c(5:8))          
Data2 <- meta2DF(meta1, title = "Thrombolytic Therapy" , 
                    rowOrder = "effect", decreasing = TRUE,
                    add = add)
Data2                                  
}
