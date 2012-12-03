\name{makeCIDesc}
\alias{makeCIDesc}
\title{
Make a column description for confidence intervals
}
\description{
The function produces descriptions that are used for making confidence interval 
columns inside the \code{newCols} argument in the \code{metaDF2Matrix()} 
function.
}
\usage{
makeCIDesc(col1 = "lower", col2 = "upper", round = 2, brackets = c("[", "]"), 
            sep=", ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col1}{Lower bound column name.}
  \item{col2}{Upper bound column name.}
  \item{round}{Rounding to nearest decimal for both columns.}
  \item{brackets}{Character vector for the type of brackets to use on either 
                  side of the numbers.}
  \item{sep}{Character to seperate the two numbers.}
}

\details{
The column names are taken from the data frame of the \code{'metaDF'} object.
It is a convenience function for specifying the new column format within the
\code{metaDF2Matrix()} function.
}
\value{
An object of class \code{'coldesc'} with a list of following components:
  \item{format}{A format string}
  \item{colnames}{A character vector of column names}
}

\seealso{
\code{\link{makeMSDDesc}},
\code{\link{makeColDesc}}
}
\examples{
makeCIDesc("lower", "upper", 2, c("[", "]"))
makeCIDesc("lower", "upper", 2, c("[", "]"), sep = " to ")
}
