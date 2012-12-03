\name{metaParDefaults}
\alias{metaParDefaults}

\title{
Default Settings for metaPar 
}
\description{
Changes default settings to metaPar.
}
\usage{
metaParDefaults(...)
}

\arguments{
  \item{\dots}{Lists of named parameter settings}
}
\details{
This function is similar as metaPar, only it changes the settings in metaPar
permenantly and will affect all subsequent plots until a new setting is 
specified. 
Valid parameters are listed in \code{\link{gpar}} for each of the named
arguments.
The named arguments are:
\tabular{ll}{
box \tab      List of parameters for the sample size box. \cr
diamond \tab  List of parameters for the overall effect diamond. \cr
lines \tab    List of parameters for the confidence intervals. \cr
refLine \tab  List of parameters for the null hypothesis line. \cr
summaryLine \tab List of parameters for the summary estimate line. \cr
heading \tab  List of parameters for the heading of the confidence interval
              graph. \cr
label \tab    List of parameters for the axis labels. \cr
axis \tab     List of parameters for the x-axis. \cr
title \tab    List of parameters for the title. \cr
subtitle \tab List of parameters for the subtitle. \cr
text \tab     List of parameters for the table texts. \cr 
hetStat \tab  List of parameters for the heterogeneity measures. \cr 
}
}
\value{
None
}
\seealso{
\code{\link{metaPar}}
}
\examples{
metaParDefaults(box=list(fill="royalblue", col="royalblue"),
                lines=list(col="darkblue"),
                diamond=list(fill="royalblue", col="royalblue"),
                heading=list(col="royalblue"),
                axis=list(cex=0.8, col="orange"),
                refLine=list(col="orange"),
                summaryLine=list(col="orange"), 
                label=list(col="royalblue"),
                text=list(col="darkblue"),
                hetStat=list(col="royalblue") 
                )
metaPar()
## draw forest plot with default settings
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")                                
Data <- meta2DF(meta1)                 
matrix <- metaDF2Matrix(Data)                          
drawMeta(matrix, plotPar=metaPar())
}
