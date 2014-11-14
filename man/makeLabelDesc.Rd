\name{makeLabelDesc}
\alias{makeLabelDesc}

\title{
Make a Text Description for Labeled Heterogeneity Measures
}
\description{
The function produces descriptions that are used for reporting
heterogeneity information in the \code{stat} argument of the 
\code{metaDF2Matrix()} function.
}
\usage{
makeLabelDesc(labelnames, label)
}
\arguments{
  \item{labelnames}{Character vector of names of labeled measures. See
    Details.} 
  \item{label}{Label printed in front of results for heterogeneity
    measures.} 
}
\details{
Valid heterogeneity measures for \code{'metabinDF'} objects include:
\code{Q, p, tau2, H, H.conf, I2, I2.conf, Q.CMH}.
}
\value{
An object of class \code{'labeldesc'} with a list of following
components: 
  \item{format}{A format string}
  \item{labelnames}{A character vector of names of labeled measures}
}
\seealso{
\code{\link{makeStatDesc}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data = Olkin95, 
                 subset = c(41,47,51,59), sm = "RR", method = "I")
Data <- meta2DF(meta1) 
het1 <- makeLabelDesc(c("I2.conf"), label = "Heterogeneity:")
het2 <- makeLabelDesc(c("Q"), label = "                     ")
matrix <- metaDF2Matrix(Data, stat = list(het1,het2))
drawMeta(matrix, plotPar = metaPar(stat = list(col = "darkred", 
                                               fontface = "bold.italic")))
}