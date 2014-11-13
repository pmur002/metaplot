\name{metaPar}
\alias{metaPar}
\title{
Set Graphical Parameters for Forest Plot
}
\description{
Wrapper function for specifying graphical parameters to forest plots.
}
\usage{
metaPar(...)
}
\arguments{
  \item{\dots}{Lists of named parameter settings}
}
\details{
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
A list of graphical parameters.
}
\examples{
metaPar(box = list(fill = "royalblue", col = "royalblue"),
                   lines = list(col = "darkblue"),
                   diamond = list(fill = "royalblue", col = "royalblue"))
}