## source("meta2DF.R")
## source("metaDF2Matrix.R")
## source("drawMeta.R")
## source("functions.r")
## source("drawMetaBasic.R")
## source("printMethods.r")
## library(metaplot)
example(drawMeta)

##=============simple example============##
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59), sm="RR", method="I")
Data <- meta2DF(meta1)
matrix <- metaDF2Matrix(Data)
drawMeta(matrix)
##=============metaplot============##
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59), sm="RR", method="I")
metaPlot(meta1)
metaPlot(Data)
metaPlot(matrix)
metaPlot(meta1, plotCol=2, plotHead="Relative Risk")
######################### viewports + borders #############################
downViewport("Graph")
current.viewport()
grid.rect()
upViewport()
# add horizontal line
pushViewport(viewport(layout.pos.row=7))
grid.lines(c(0,1), c(0,0), gp=gpar(col="royalblue", lty=2, lwd=2))
popViewport()
# change base viewport name
drawMeta(matrix, vpName="MyForest1")
current.vpTree()
# draw border
pushViewport(viewport(layout.pos.row=seq(2,9)))
grid.rect()
popViewport()
# add more texts
downViewport("text.X.study.7.1")
grid.text("Overall Measures", just="left", x=0, gp=gpar(col="red"))
# add background
##1
grid.newpage()
grid.rect(gp=gpar(col=NA, fill="navy"))
drawMeta(matrix, newpage=FALSE,
         plotPar = metaPar(box=list(fill="yellow", col="yellow"),
                           lines=list(col="yellow"),
                           diamond=list(fill="yellow", col="yellow"),
                           text=list(col="yellow"),
                           axis=list(col="yellow"),
                           stat=list(col="yellow")))
##1
drawMeta(matrix)
downViewport("Forest")
pushViewport(viewport(layout.pos.row=6))
grid.rect(gp=gpar(col=NA, fill="grey"))
upViewport(0)
drawMeta(matrix, newpage=FALSE)
##2
drawMeta(matrix)
downViewport("text.X59.effect.6.2")
grid.rect(gp=gpar(col=NA, fill="grey"))
lab <- grid.get("text.X59.effect", grep=TRUE)
grid.draw(lab)
upViewport(0)

############################### makeText ##################################
#===== use own texts ====#
makeRowText(text = "Heterogeneity Test: p=0.26")
matrix <- metaDF2Matrix(Data, stat=list("Test for Heterogeneity: p=0.26"))
drawMeta(matrix)
#===== use customised format ====#
textdesc <- makeStatDesc(c("I2", "p"), "Hetero: I2 = %f; p = %f")
makeRowText(textdesc, Data$hetero)
matrix <- metaDF2Matrix(Data, stat=list(textdesc))
drawMeta(matrix)
#===== use default formats ====#
het1 <- makeLabelDesc(c("I2.conf"), label = "Heterogeneity:")
#~~ make metabin stat labels
heteroStat <- metabinStat(Data)
makeRowText(het1, heteroStat)
## 2 lines or more
het2 <- makeLabelDesc(c("Q"), label = "                     ")
matrix <- metaDF2Matrix(Data, stat=list(het1,het2))
drawMeta(matrix, plotPar = metaPar(stat=list(col="darkred",
                                             fontface="bold.italic")))
#====== no hetero stat =====#
matrix <- metaDF2Matrix(Data, stat=NULL)
drawMeta(matrix)
## empty line after summary
matrix <- metaDF2Matrix(Data, stat=list(""))
drawMeta(matrix)
############################### metaPar ##################################
metaParDefaults(box=list(fill="royalblue", col="royalblue"),
                lines=list(col="darkblue"),
                diamond=list(fill="royalblue", col="royalblue"),
                heading=list(col="royalblue"),
                axis=list(cex=0.8, col="orange"),
                refLine=list(col="orange"),
                summaryLine=list(col="orange"),
                label=list(col="royalblue"),
                text=list(col="darkblue"),
                stat=list(col="royalblue")
                )
metaParDefaults(box = list(fill="steelblue"), stat = list(fontface="plain"))
metaPar()
resetDefaults()
metaPar()
metaPar(label=list(cex=0.5, fontface="italic"), heading=list(cex=2),
        text=list(col="blue"))
drawMeta(matrix, plotPar = metaPar(text=list(cex=2)))
############################### makeColDesc #################################
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59), sm="RR", method="I")
forestDF <- meta2DF(meta1)
coldesc <- makeColDesc("Study%s (%.0f,%.0f)", c("study", "n.e", "n.c"))
studynum <- makeCol(coldesc, forestDF$DF)
studynum
############################### makeCIDesc #################################
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59), sm="RR", method="I")
forestDF <- meta2DF(meta1)
coldesc <- makeCIDesc("lower", "upper", 2, c("[", "]"))
ci <- makeCol(coldesc, forestDF$DF)
ci
############################### makeMSDDesc #################################
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont,
                  sm="SMD")
forestDF <- meta2DF(meta1, title = "Mental Health Treatment",
                       rowOrder = "effect", decreasing = TRUE)

coldesc <- makeMSDDesc("mean.e", "sd.e", c(1,2), c(" (", ")"))
msd <- makeCol(coldesc, forestDF$DF)
msd
############################### printMethods #################################
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59), sm="RR", method="I")
Data <- meta2DF(meta1)
print(Data)
#######################  metabin: binary data  ##########################

#confidence interval plot customisations
drawMeta(matrix,
         plotCol = 2,
         refLine = log(2),
         boxSize = 0.75,
         plotWidth = unit(2,"inches"),
         plotHead = "Relative risk (log scale)")

##illustrative example
library(meta)
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59), sm="RR", method="I")
 #testing 'add' argument
add <- list(test1 = c(1:4), test2 = c(5:8))
Data <- meta2DF(meta1, title = "Thrombolytic Therapy",
                subtitle = "Olkin(1995)",
                rowOrder = "effect", decreasing = TRUE,
                add = add)
matrix <- metaDF2Matrix(Data,
                         order = c("study", "event.e", "event.c", "effect",
                                  "ci", "w.fixed", "w.random"),
                         roundCols = c("effect" = 2, "w.fixed" = 1,
                                        "w.random" = 1),
                         hgap = c(3, 9, 12),
                         newCols=list(ci=makeCIDesc("e.lower", "e.upper", 2,
                         c("[", "]")), test3=c(1:4)),
                         colNames = c("Study", "Experimental Events",
                                      "Control Events", "Relative Risk",
                                      "CI (95%)", "Weight(fixed)",
                                      "Weight(random)") )
drawMeta(matrix,
         plotCol = 6,
         clip=log(c(0.05,6)),
         xlab = c("Favours treatment","Favours control"),
         plotHead = "Relative risk (log scale)",
         plotPar = metaPar(box=list(fill="royalblue", col="royalblue"),
                           lines=list(col="darkblue"),
                           diamond=list(fill="royalblue", col="royalblue"),
                           title=list(cex=2)))

###################### metacont: single group ########################

##simple example
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont,
                  sm="SMD")

Data <- meta2DF(meta1)
matrix <- metaDF2Matrix(Data,
                        order = c("study", "effect"),
                        roundCols = c("effect" = 2),
                        hgap = 2)
drawMeta(matrix,
         plotCol = 3,
         plotHead = "Weighted Mean Difference")

##illustrative example
library(meta)
data(Fleiss93cont)
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont,
                  sm="SMD")

Data <- meta2DF(meta1, title = "Mental Health Treatment",
                subtitle = "Fleiss(1993)",
                rowOrder = "effect", decreasing = TRUE)
matrix <- metaDF2Matrix(Data,
                        order = c("study", "msd.e", "msd.c", "effect",
                                  "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1,
                                       "w.random" = 1),
                        hgap = c(3, 12), vgap = 5)
drawMeta(matrix,
         plotCol = 6,
         plotHead = "Weighted Mean Difference")
###################### metacont: grouped studies #######################
library(meta)
data(Fleiss93cont)
meta <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont,
                 sm="SMD", byvar=c(1,2,1,1,2), bylab="group")

Data <- meta2DF(meta, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE)
matrix <- metaDF2Matrix(Data, groupLab = c("Group One", "Group Two", "Overall"),
                        order = c("study", "msd.e", "msd.c", "effect",
                                  "ci", "w.fixed", "w.random"),
                        roundCols = c("effect" = 2, "w.fixed" = 1,
                                      "w.random" = 1),
                        hgap=c(2,11,18), vgap = 5)
drawMeta(matrix,
         plotCol = 6,
         plotHead = "Weighted Mean Difference")


#==============================================================================#
# plot group meta
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95,
                 subset=c(41,47,51,59),
                 sm="RR", method="I")
forest(meta1, byvar=c(1,2,1,2), bylab="label")
