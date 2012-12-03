\name{print.metabinDF}
\alias{print.metabinDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print Method for 'metabinDF' Objects}
\description{
Print method for objects of class \code{'metabinDF'}.
}
\usage{
\method{print}{metabinDF}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of \code{'metabinDF'}.}
  \item{...}{Other arguments not used.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\seealso{
\code{\link{meta2DF.metabin}},
\code{\link{metaDF2Matrix.metabinDF}}
}
\examples{
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")                                
Data <- meta2DF(meta1)  
print(Data)
}