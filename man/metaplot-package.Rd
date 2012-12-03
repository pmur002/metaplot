\name{metaplot-package}
\alias{metaplot-package}
\alias{metaplot}
\docType{package}
\title{
Metaplot: A Forest Plot Graphics Package 
}
\description{
Draws flexible forest plots. Provides methods for meta-analysis objects from 
\pkg{rmeta}, \pkg{meta}, and \pkg{metafor} packages. 
}
\details{
\tabular{ll}{
Package: \tab metaplot\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-06-18\cr
License: \tab GPL-2\cr
}
Draws forest plots with control over row and column order, position and width of
confidence interval graph, graph and axis labels, rounding of numeric values,
adding new columns, combining columns, adding horizontal and vertical spaces,
specifying reference line, customising plotting parameters, drawing graph on
logged scale, alignment of columns.

It also utilises the naming scheme in forest plots, where users can use
\code{grid.edit()} to modify specific aspects of the plot after it has been 
drawn without needing extensive knowledge of the structure of the plot.  

The process of producing the plot from a meta-analysis object requires 3 
functions in this package. First, \code{\link{meta2DF}} converts a meta-analysis 
object into a data frame, then \code{\link{metaDF2Matrix}} converts the 
data frame into a matrix. Lastly, \code{\link{drawMeta}} draws the forest plot 
using the matrix. 
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{Paul Murrell, David Scott, and Edna Lu 

Maintainer: Edna Lu <elu005@aucklanduni.ac.nz>
%~~ The author and/or maintainer of the package ~~
}
\references{
Paul Murrell (2005). \emph{R Graphics}. Chapman & Hall/CRC.

Ried, K. (2006). 
Interpreting and understanding meta-analysis graphs - A practical guide. 
\emph{Australian Family Physician, 35}(8).

Note:
The code for \code{drawMeta()} generic function is based on Thomas Lumley's 
\code{forestplot()} function from the \pkg{rmeta} package.
} 
\keyword{ package }
\examples{
example(drawMeta)
}
