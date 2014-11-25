\name{metaDF2Matrix.metabinDF}
\alias{metaDF2Matrix.metabinDF}

\title{
Convert 'metacontDF' Objects to Matrices
}
\description{
A function that converts \code{'metabinDF'} objects into a character
matrix used for plotting.
}

\usage{
\method{metaDF2Matrix}{metabinDF}(df,
             order, newCols = NULL, roundCols = NULL,
             stats = list(hetero = makeStatsDesc(labelNames =
                                                 c("I2", "tau2", "p"))),
             groupStats = NULL, newLabel = NULL,
             colNames, groupLab, hgap = NULL, vgap, drop, ...)
}
\arguments{
  \item{df}{An object of class \code{'metabinDF'}.}
  \item{newCols}{A list of new columns made from existing columns in the
    data frame. Can use functions \code{\link{makeCIDesc}} and
    \code{\link{makeMSDDesc}} to combine and round columns.}
  \item{order}{A vector of column names in the order that it should
    appear in the matrix.}
  \item{roundCols}{A named vector with values of rounding to the nearest
    decimal place of specified column names.}
  \item{colNames}{A vector of column names for the ordered matrix.}
  \item{hgap}{A vector of row numbers to add horizontal spaces.}
  \item{vgap}{A vector of column numbers to add vertical spaces.}
  \item{stat}{A list of descriptions or strings for heterogeneity
    statistics.  Can use the functions \code{\link{makeStatsDesc}} and
    \code{\link{makeLabelDesc}} to make descriptions that specify format
    and select relevant statistics.}
  \item{\dots}{Not used.}
}
\details{
  Rounding can be applied using the \code{roundCols} argument for all
  numeric columns except for the confidence interval column which must
  be done through the \code{newCols} argument using the
  \code{\link{makeCIDesc}} function.
}
\value{
An object of class \code{'metabinM'} and \code{'metaM'} with following
list of components:
  \item{Matrix}{A matrix set out as to be drawn in the forest plot.}
  \item{PlotDF}{A data frame with plotting information for drawing the
    confidence interval graph.}
}

\seealso{
  \code{\link{meta2DF.metabin}}, \code{\link{drawMeta.metabinM}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data = Olkin95,
                 subset = c(41,47,51,59), sm = "RR", method = "I")
Data <- meta2DF(meta1, title = "Thrombolytic Therapy",
                rowOrder = "effect", decreasing = TRUE)
matrix <- metaDF2Matrix(Data,
                        order = c("study", "event.e", "event.c",
                                  "effect", "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2,
                                      "w.fixed" = 1, "w.random" = 1),
                        hgap = c(2, 8, 11), vgap = 5,
                        newCols = list(ci = makeCIDesc("e.lower", "e.upper",
                                                        2, c("[", "]"))))
matrix
}