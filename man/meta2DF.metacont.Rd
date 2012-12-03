\name{meta2DF.metacont}
\alias{meta2DF.metacont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert 'metacont' Objects into Data Frames
}
\description{
A function that turns objects of class \code{'metacont'} into a list of data frames 
used for plotting. 
}
\usage{
\method{meta2DF}{metacont}(meta, add = NULL, rowOrder = NULL, title = NULL, 
                           subtitle = NULL, ...)
}
\arguments{
  \item{meta}{An object of class \code{'metacont'}}.
  \item{add}{List of vectors that can be added as columns to the data frame with 
             corresponding values to each study row.}
  \item{rowOrder}{Column name to order rows of studies by. }
  \item{title}{Title of the meta-analysis to be shown on top of the plot}
  \item{subtitle}{Subtitle of the meta-analysis to be shown under the title}
  \item{\dots}{Further arguments from \code{order}.}
}
\details{
In order to produce the plot, \code{\link{metaDF2Matrix.metacontDF}} must be 
called after producing the \code{'metacontDF'} object from this function. 
The data frame columns can be selected and ordered using 
\code{\link{metaDF2Matrix.metacontDF}}. 
}
\value{
An object of class \code{'metacontDF'} and \code{'metaDF'} with a list of 
following components:
  \item{DF}{A data frame with studies as rows and study attributes as columns.}
  \item{summary.fixed}{A data frame with attributes of fixed effects model.}
  \item{summary.random}{A data frame with attributes of the random effects model.}
  \item{hetero}{Heterogenity- Q statistic, degrees of freedom, p-value.}
  \item{title}{Title of the meta-analysis.}
  \item{subtitle}{Subtitle of the meta-analysis.}
}
\seealso{
\code{\link{metaDF2Matrix.metacontDF}},
\code{\link{drawMeta.metacontM}}
}
\examples{
## single group 
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD")
Data <- meta2DF(meta1, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE) 
## grouped studies
library(meta)
data(Fleiss93cont)
meta <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD", byvar=c(1,2,1,1,2), bylab="group") 
Data <- meta2DF(meta, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE)
} 