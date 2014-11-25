\name{makeLabelDesc}
\alias{makeLabelDesc}

\title{
  Make a label description for heterogeneity measures.
}
\description{

  This function produces descriptions that are used for making a label,
  which is a heterogeneity measure in a specified format in
  character. It is used inside the arguments \code{stats} and
  \code{newLabel} in the function \code{\link{metaDF2Matrix}}.

}

\usage{
makeLabelDesc(format, heteroNames)
}
\arguments{
  \item{format}{Format string for new label.}
  \item{heteroNames}{Names of existing heterogeneity measures.}
}
\details{
  The names of heterogeneity measures are taken from the vector, "hetero",
  of the \code{"metaDF"} object.
}
\value{
An object of class \code{'labeldesc'} with a list of following components:
  \item{format}{A format string}
  \item{labelnames}{A character vector of names of labeled measures}
}
\seealso{
\code{\link{makeStatsDesc}}
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
drawMeta(matrix,
         plotPar = metaPar(stat = list(col="darkred",
                                       fontface="bold.italic")))
}