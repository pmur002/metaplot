\name{makeColDesc}
\alias{makeColDesc}

\title{
Make a Column Description in Specified Format 
}
\description{  
The function produces descriptions that are used for making a new column
inside the \code{newCols} argument in the \code{metaDF2Matrix()}
function.
}
\usage{
makeColDesc(format, colnames)
}
\arguments{
  \item{format}{Format string.}
  \item{colnames}{Column names}
}
\details{ 
The column names are taken from the data frame of the \code{"metaDF"}
object.  It is a convenience function for specifying the new column
format within the \code{metaDF2Matrix()} function.
}
\value{
  An object of class \code{'coldesc'} with a list of following
  components:   
  \item{format}{A format string}
  \item{colnames}{A character vector of column names}
}
\seealso{
\code{\link{makeCIDesc}}, \code{\link{makeMSDDesc}}
}
\examples{
makeColDesc("Study\%s (\%.0f,\%.0f)", c("study", "n.e", "n.c"))
}