\name{print.metabinDF}
\alias{print.metabinDF}

\title{Print Method for 'metabinDF' Objects}
\description{
Print method for objects of class \code{'metabinDF'}.
}
\usage{
\method{print}{metabinDF}(x, ...)
}

\arguments{
  \item{x}{An object of \code{'metabinDF'}.}
  \item{...}{Other arguments not used.}
}
\seealso{
\code{\link{meta2DF.metabin}},
\code{\link{metaDF2Matrix.metabinDF}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data = Olkin95,
                 subset = c(41,47,51,59), sm = "RR", method = "I")
Data <- meta2DF(meta1)
print(Data)
}