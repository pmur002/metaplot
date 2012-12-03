\name{makeStatDesc}
\alias{makeStatDesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a text description for heterogeneity measures 
}
\description{
The function produces descriptions that are used for reporting
heterogeneity information in the \code{hetStat} argument of the 
\code{metaDF2Matrix()} function.
}
\usage{
makeStatDesc(statnames, format)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statnames}{Character vector of names of measures. See Details.}
  \item{format}{Format string specifying customised format.}
}
\details{
The names of measure are taken from the \code{'hetero'} vector in 
the \code{"metaDF"} object. 
Valid heterogeneity measures for \code{'metabinDF'} objects include:
\code{Q, p, tau2, H, H.lower, H.upper, I2, I2.lower, I2.upper, Q.CMH}.
}
\value{
An object of class \code{'statdesc'} with a list of following components:
  \item{format}{A format string}
  \item{statnames}{A character vector of names of measures}
}
\seealso{
\code{\link{makeLabelDesc}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")                                
Data <- meta2DF(meta1) 
statdesc <- makeStatDesc(c("I2", "p"), "Hetero: I2 = \%f; p = \%f")
matrix <- metaDF2Matrix(Data, stat=list(statdesc))                                                
drawMeta(matrix)
## no hetero stats
matrix <- metaDF2Matrix(Data, stat=NULL)                                                
drawMeta(matrix)
## empty line after summary
matrix <- metaDF2Matrix(Data, stat=list(""))
drawMeta(matrix) 
}