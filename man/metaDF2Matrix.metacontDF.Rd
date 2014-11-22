\name{metaDF2Matrix.metacontDF}
\alias{metaDF2Matrix.metacontDF}
\title{
Convert 'metabinDF' Objects to Matrices
}
\description{
A function that converts \code{'metacontDF'} objects into a character matrix
used for plotting.
}
\usage{
\method{metaDF2Matrix}{metacontDF}(df,
             order, newCols = NULL,
             roundCols = NULL,
             stats = list(hetero = makeStatsDesc(labelNames =
                                                 c("Q", "p", "df"))),
             groupStats = NULL, newLabel = NULL, colNames,
             groupLab, hgap = NULL, vgap, drop, ...)
}
\arguments{
  \item{df}{An object of class \code{'metacontDF'}.}

  \item{newCols}{A list of new columns made from existing columns in the
    data frame. Can use functions \code{\link{makeCIDesc}} and
    \code{\link{makeMSDDesc}} to combine and round columns.}
  \item{order}{A vector of column names in the order that it should
    appear in the matrix.}
  \item{roundCols}{A named vector with values of rounding to the nearest
    decimal place of specified column names.}
  \item{colNames}{A vector of column names for the ordered matrix.}
  \item{groupLab}{A vector of labels for subgroups and overall effect.}
  \item{hgap}{A vector of row numbers to add horizontal spaces.}
  \item{vgap}{A vector of column numbers to add vertical spaces.}
  \item{\dots}{Not used.}
}
\details{
Rounding can be applied using the \code{roundCols} argument for all numeric
columns except for the mean-standard deviation and confidence interval columns
which must be done through the \code{newCols} argument using \code{makeMSDDesc}
and \code{\link{makeCIDesc}} functions.
}
\value{
An object of class \code{'metacontM'} and \code{'metaM'} with a list
of following components:
  \item{Matrix}{A matrix set out as to be drawn in the forest plot.}
  \item{PlotDF}{A data frame with plotting information for drawing the
    confidence interval graph.}

}
\seealso{
\code{\link{meta2DF.metacont}},
\code{\link{drawMeta.metacontM}}
}
\examples{
### metacont: single group
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
                  data = Fleiss93cont, sm = "SMD")

Data <- meta2DF(meta1, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE)
matrix <- metaDF2Matrix(Data,
                        order = c("study", "msd.e", "msd.c", "effect",
                                  "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1,
                                      "w.random" = 1),
                        hgap = c(2, 11), vgap = 5)
### metacont: grouped studies
meta <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
                 data = Fleiss93cont, sm = "SMD",
                 byvar = c(1,2,1,1,2), bylab = "group")
Data <- meta2DF(meta, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE)
matrix <- metaDF2Matrix(Data,
                        groupLab = c("Group One", "Group Two", "Overall"),
                        order = c("study", "msd.e", "msd.c", "effect",
                                  "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1,
                                      "w.random" = 1),
                        hgap=c(2,10,16), vgap = 5)
str(matrix)
}