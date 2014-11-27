\name{meta2DF}
\alias{meta2DF}

\title{
  Convert Meta-analysis Objects to Data Frames
}
\description{
  A generic function that turns meta-analysis objects into a list of
  data frames used for plotting.
}
\usage{
meta2DF(meta, ...)
}
\arguments{
  \item{meta}{An object of class \code{metabin}, \code{'metacont'},
    \code{meta.MH}, \code{meta.DSL}, \code{rma.mh}, \code{rma.peto}, or
    \code{rma.uni}. See 'Details'.}
  \item{\dots}{Other arguments.}
}
\details{
  Currently, object \code{meta} can come from functions \code{metabin()}
  or \code{metacont()} from the \pkg{meta} package, \code{meta.DSL} or
  \code{meta.MH} from the \pkg{rmeta} package, or \code{rma.mh},
  \code{rma.peto}, or \code{rma.uni} from the \pkg{metafor} package.
}
\value{
  An object of class \code{c('metabinDF', 'metaDF')} or
  \code{c('metacontDF', 'metaDF')}. For details of these classes, see
  \code{\link{meta2DF.metabin}} and \code{\link{meta2DF.metacont}}
  respectively.
}
\seealso{
\code{\link{meta2DF.metabin}},
\code{\link{meta2DF.metacont}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data = Olkin95,
                 subset = c(41,47,51,59), sm = "RR", method = "I")
Data1 <- meta2DF(meta1)
Data1
### testing 'add' argument
add <- list(test1 = c(1:4), test2 = c(5:8))
Data2 <- meta2DF(meta1, title = "Thrombolytic Therapy" ,
                 rowOrder = "effect", decreasing = TRUE,
                 add = add)
Data2
}
