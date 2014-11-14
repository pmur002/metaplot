\name{makeMSDDesc}
\alias{makeMSDDesc}
\title{
Make a Column Description for Combined Mean and Standard Deviation
}
\description{
The function produces descriptions that are used for making mean and
standard deviation combined columns inside the \code{newCols} argument
in the \code{metaDF2Matrix()} function.
}
\usage{
makeMSDDesc(col1 = "mean", col2 = "sd",
            round = c(1, 2), brackets = c("(", ")"))
}
\arguments{
  \item{col1}{Column name for mean.}
  \item{col2}{Column name for standard deviation.}
  \item{round}{Numeric vector specyfying rounding for each column.}
  \item{brackets}{Character vector for the type of brackets to use on
    either side of the standard deviation.}
}
\details{
The column names are taken from the data frame of the \code{"metaDF"}
object.  It is a convenient function for specifying the new column
format within the \code{metaDF2Matrix()} function.
}
\value{
  An object of class \code{"coldesc"} with a list of following
  components: 
  \item{format}{A format string}
  \item{colnames}{A character vector of column names}
}
\seealso{
\code{\link{makeCIDesc}}, \code{\link{makeColDesc}}
}
\examples{
makeMSDDesc("mean.e", "sd.e", c(1, 2), c(" (", ")"))
}