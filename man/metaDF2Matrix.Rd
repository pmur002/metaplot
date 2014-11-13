\name{metaDF2Matrix}
\alias{metaDF2Matrix}

\title{
Convert 'metaDF' Objects to Matrices
}
\description{
A generic function that turns \code{'metaDF'} objects into a character matrix
used for plotting.
}
\usage{
metaDF2Matrix(df, ...)
}
\arguments{
  \item{df}{Either an object of class \code{'metabinDF'}, or an object
    of class \code{'metacontDF'}. See 'Details'.}
  \item{\dots}{Other arguments.}
}
\details{
\code{df} can be any object from the generic function
\code{meta2DF()}. It is converted into a matrix for plotting by the
function \code{drawMeta()}.
}
\seealso{
\code{\link{metaDF2Matrix.metabinDF}},
\code{\link{metaDF2Matrix.metacontDF}},
\code{\link{drawMeta}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data = Olkin95,
                 subset = c(41,47,51,59), sm = "RR", method = "I")
Data <- meta2DF(meta1, title = "Thrombolytic Therapy" ,
                rowOrder = "effect", decreasing = TRUE)
matrix <- metaDF2Matrix(Data,
                        order = c("study", "event.e", "event.c", "effect",
                                  "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1,
                                      "w.random" = 1),
                        hgap = c(2, 8, 11), vgap = 5,
                        newCols = list(ci = makeCIDesc("e.lower", "e.upper",
                                                        2, c("[", "]"))))
matrix
}
