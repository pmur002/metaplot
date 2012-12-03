\name{resetDefaults}
\alias{resetDefaults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reset Default Settings for metaPar 
}
\description{
A function that resets any user specified default settings using 
\code{\link{metaParDefaults}} to the original default settings.
}
\usage{
resetDefaults()
}

\seealso{
\code{\link{metaParDefaults}},
\code{\link{metaPar}}
}
\examples{
metaParDefaults(box = list(fill="steelblue"), hetStat = list(fontface="plain"))        
## defaults changed
metaPar()
resetDefaults()
## back to original defaults
metaPar()
}
