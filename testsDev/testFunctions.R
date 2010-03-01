source("../R/functions.R")

library(codetools)
cleanProg <- function(realResult,theoResult="",result=TRUE,tolerance=0){
  functionNames <- strsplit(deparse(substitute(realResult)),"\\(")[[1]][1]
  if(identical(theoResult,"")==FALSE){
    if( isTRUE(all.equal( realResult , theoResult ))!=result ){
      cat("WARNING(PreTest2) in    ",functionNames,":",deparse(substitute(realResult)), " == ",theoResult," is not ",result,"\a\n\a")
    }
  }else{}
  if(length(findGlobals(get(functionNames),FALSE)$variables)  > tolerance){
    cat("WARNIGS(detectGlobal) in ",functionNames,": These are the globals:",findGlobals(get(functionNames),FALSE)$variables,"\a\n")
  }else{}
}


f1 <- as.factor(LETTERS[floor(runif(100,1,3))])
f1b <- as.factor(LETTERS[floor(runif(100,1,3))])
f2 <- as.factor(LETTERS[floor(runif(100,1,5))])
f3 <- as.factor(LETTERS[floor(runif(100,1,10))])

o1 <- as.ordered(as.factor(LETTERS[floor(runif(100,1,3))]))
o2 <- as.ordered(as.factor(LETTERS[floor(runif(100,1,5))]))
o3 <- as.ordered(as.factor(LETTERS[floor(runif(100,1,10))]))

i1 <- d1 <- as.numeric(floor(runif(100,1,3)))
i2 <- d2 <- as.numeric(floor(runif(100,1,5)))
i3 <- d3 <- as.numeric(floor(runif(100,1,10)))
class(d1) <- c("discrete","numeric")
class(d2) <- c("discrete","numeric")
class(d3) <- c("discrete","numeric")
class(i1) <- "integer"
class(i2) <- "integer"
class(i3) <- "integer"

y1 <- rnorm(100)
y2 <- rnorm(100,1:100)
y3 <- rnorm(100,1:100)

df <- data.frame(f1,i1,f2,o2,y2,y1)


malade <- rep(c("Vivant","Mort"),c(49,51))
traitement <- rep(c("A","B","A","B"),c(22,31,27,20))

concours <- rep(c("Reussite","Echec"),c(40,160))
sexe <- rep(c("H","F","H","F"),c(10,30,20,140))
bacType <- rep(c("A","B","C","D","A","B","C","D"),c(0,1,35,4,20,40,60,40))
bacType2 <- rep(c("A","B","C","D","H","F","St","E","M","Sttk"),20)
cheveux <- c("noir","blond","chatain","roux")[floor(runif(200,1,5))]

bacMension <- ordered(rep(c("P","AB","B","TB","P","AB","B","TB"),c(5,5,15,13,95,45,15,7),
                          levels=c("P","AB","B","TB")))
bacMension2 <- ordered(rep(c("P","P+","AB","AB+","B","B+","TB","TB+","P","P+","AB","AB+","B","B+","TB","TB+"),c(2,3,3,4,7,8,9,4,40,45,20,25,18,5,3,4),
                           levels=c("P","P+","AB","AB+","B","B+","TB","TB+")))

nbRedoublement <- rpois(200,0.5)
nbRedoublement2 <- rpois(200,3)
nbRedoublement2[nbRedoublement2>8]<-8
class(nbRedoublement) <- c("discrete","numeric")
class(nbRedoublement2) <- c("discrete","numeric")

taille <- rnorm(200,170,5)
poids <- rnorm(200,65,10)

###############################################################
###      Utility functions to write out pieces of text      ###
###                    Both univ and biv                    ###
###############################################################

####################
#                  #
#   Latex output   #
#                  #
####################

cleanProg(r2lEol)
r2lEol("my line")

cleanProg(r2lComment)
r2lComment("To comment",out="latex")

cleanProg(r2lStartTable)
r2lStartTable(attrs="|c|cc|",out="latex")

cleanProg(r2lEndTable)
r2lEndTable(out="latex")

cleanProg(r2lBuildRow)
r2lBuildRow(c("case1","case2"),span=c(1,1),out="latex")
r2lBuildRow(c("case1","case2"),span=c(1,2),out="latex")
r2lBuildRow(c("case1","case2"),span=c(2,3),hline=FALSE,out="latex")

cleanProg(r2lBuildColumnTitle)
r2lBuildColumnTitle("MyTitle",span=1,out="latex")
r2lBuildColumnTitle(c("MyTitle","Boxplot","Barplot"),span=c(1,2,1),out="latex")

cleanProg(r2lPercentRow)
r2lPercentRow(1/3,out="latex")

cleanProg(r2lIncludeGraphics)
r2lIncludeGraphics("toto.eps",out="latex")

cleanProg(r2lBold)
r2lBold("ert",out="latex")

cleanProg(r2lItal)
r2lItal("aze",out="latex")

cleanProg(r2lStartBlock)
r2lStartBlock("zer",out="latex")

cleanProg(r2lEndBlock)
r2lEndBlock(out="latex")

cleanProg(r2lEndStruct)
r2lEndStruct(out="latex")


###################
#                 #
#   Html output   #
#                 #
###################

cleanProg(r2lEol)
r2lEol("my line")

cleanProg(r2lComment)
r2lComment("To comment",out="html")

cleanProg(r2lStartTable)
r2lStartTable(attrs="|c|cc|",out="html")

cleanProg(r2lEndTable)
r2lEndTable(out="html")

cleanProg(r2lBuildRow)
r2lBuildRow(c("case1","case2"),span=c(1,1),out="html")
r2lBuildRow(c("case1","case2"),span=c(1,2),out="html")
r2lBuildRow(c("case1","case2"),span=c(2,3),hline=FALSE,out="html")

cleanProg(r2lPercentRow)
r2lPercentRow(1/3,out="html")

cleanProg(r2lBuildColumnTitle)
r2lBuildColumnTitle("MyTitle",span=1,out="html")
r2lBuildColumnTitle(c("MyTitle","Boxplot","Barplot"),span=c(1,2,1),out="html")

cleanProg(r2lIncludeGraphics)
r2lIncludeGraphics("toto.eps",out="html")

cleanProg(r2lBold)
r2lBold("ert",out="html")

cleanProg(r2lItal)
r2lItal("aze",out="html")

cleanProg(r2lStartBlock)
r2lStartBlock("zer",out="html")

cleanProg(r2lEndBlock)
r2lEndBlock(out="html")

cleanProg(r2lEndStruct)
r2lEndStruct(out="html")



###############################################################
###                    Functions for Univ                   ###
###############################################################

####################
#                  #
#   Latex output   #
#                  #
####################

cleanProg(r2lUnivBeginStruct)
cat(r2lUnivBeginStruct(f1,nbColum=3,tabSpec="|c|cc|",out="latex"))

cleanProg(r2lUnivFrequency)
cat(r2lUnivFrequency(f1,out="latex"))
cat(r2lUnivFrequency(f2,out="latex"))

cleanProg(r2lUnivSummary)
cat(r2lUnivSummary(y1,out="latex"))


###################
#                 #
#   Html output   #
#                 #
###################

cleanProg(r2lUnivBeginStruct)
cat(r2lUnivBeginStruct(f1,nbColum=3,tabSpec="|c|cc|",out="html"))

cleanProg(r2lUnivFrequency)
cat(r2lUnivFrequency(f1,out="html"))
cat(r2lUnivFrequency(f2,out="html"))

cleanProg(r2lUnivSummary)
cat(r2lUnivSummary(y1,out="html"))



###############################################################
###                     Functions for Biv                   ###
###############################################################

####################
#                  #
#   Latex output   #
#                  #
####################

cleanProg(r2lBivBeginStruct)
cat(r2lBivBeginStruct(y1,f1,nbColumn=3,tabSpec="ccc"))

cleanProg(r2lBivBannerStr)
cat(r2lBivBannerStr(y1))

cleanProg(r2lBivSummaryArray)
cat(r2lBivSummaryArray(y1,f1))
cat(r2lBivSummaryArray(y1,f2))

cleanProg(r2lBivSummary)
cat(r2lBivSummary(y1,f1))
cat(r2lBivSummary(y1,f2))

cleanProg(r2lBivQuartilesArray)
r2lBivQuartilesArray(o1,f1)
r2lBivQuartilesArray(o1,f2)

cleanProg(r2lBivQuartilesTable)
cat(r2lBivQuartilesTable(o1,f1))
cat(r2lBivQuartilesTable(o1,f2))

cleanProg(r2lBivContingencyTable)
cat(r2lBivContingencyTable(f1,f2,out="latex"))



###################
#                 #
#   Html output   #
#                 #
###################

cleanProg(r2lBivBeginStruct)
#cat(r2lBivBeginStruct(y1,f1,nbColumn=3,typeVar="Continuous vs Nominal (2)",defineTab="|c|cc|",out="html"))

cleanProg(r2lBivSummary)
cat(r2lBivSummary(y1,f1,out="html"))
cat(r2lBivSummary(y1,f2,out="html"))

cleanProg(r2lBivContingencyTable)
cat(r2lBivContingencyTable(f1,f2,out="html"))

cleanProg(r2lBivQuartilesTable)
cat(r2lBivQuartilesTable(o1,f1,out="html"))
cat(r2lBivQuartilesTable(o1,f2,out="html"))


###############################################################
###                           tests                         ###
###############################################################

cleanProg(r2lSignificance)
cat(r2lSignificance(0.2))
cat(r2lSignificance(0.005))

cleanProg(r2lPValueStr)
cat(r2lPValueStr(0.2))
cat(r2lPValueStr(0.005))
cat(r2lPValueStr(0.001))
cat(r2lPValueStr(0.0001))
cat(r2lPValueStr(0.000012345))
cat(r2lPValueStr(0.00000012345))
cat(r2lPValueStr(0.000000000000000012345))

####################
#                  #
#   Latex output   #
#                  #
####################

cleanProg(r2lBivTestCorPearson)
cat(r2lBivTestCorPearson(y1,y2,out="latex"))
cat(r2lBivTestCorPearson(y2,y3,out="latex"))

cleanProg(r2lBivTestCorSpearman)
cat(r2lBivTestCorSpearman(y1,y2,out="latex"))
cat(r2lBivTestCorSpearman(y2,y3,out="latex"))

cleanProg(r2lBivTestStudent)
cat(r2lBivTestStudent(y1,f1,out="latex"))

cleanProg(r2lBivTestWilcoxon)
cat(r2lBivTestWilcoxon(y1,f1,out="latex"))

cleanProg(r2lBivTestAnova)
cat(r2lBivTestAnova(y1,f2,out="latex"))

cleanProg(r2lBivTestKruskalWallis)
cat(r2lBivTestKruskalWallis(y1,f2,out="latex"))

cleanProg(r2lBivTestKhi2)
cat(r2lBivTestKhi2(f1,f2,out="latex"))
cat(r2lBivTestKhi2(f1,f1b,out="latex"))

cleanProg(r2lBivTestFisherExact)
cat(r2lBivTestFisherExact(f1,f2,out="latex"))
cat(r2lBivTestFisherExact(f1,f1b,out="latex"))

cleanProg(r2lBivTestOddsRatio)
cat(r2lBivTestOddsRatio(f1,f1b,out="latex"))

cleanProg(r2lBivTestRelativeRisk)
cat(r2lBivTestRelativeRisk(f1,f1b,out="latex"))

cleanProg(r2lBivTest)
cat(r2lBivTest(y1,f1,test="Student",line=c(T,F),out="latex"))
cat(r2lBivTest(y1,f2,test="Anova",line=c(F,T),out="latex"))
cat(r2lBivTest(y1,o1,test=c("Anova","CorPearson"),line=c(T,F,F),out="latex"))
cat(r2lBivTest(y1,o1,test=c("Anova","CorPearson"),line=c(T,T,T),out="latex"))
cat(r2lBivTest(y1,y2,test="CorPearson",line=c(T,F),out="latex"))
cat(r2lBivTest(f1,f1b,test=c("Khi2","OddsRatio"),line=c(F,F,F),out="latex"))


###################
#                 #
#   Html output   #
#                 #
###################

cleanProg(r2lBivTestCorPearson)
cat(r2lBivTestCorPearson(y1,y2,out="html"))
cat(r2lBivTestCorPearson(y2,y3,out="html"))

cleanProg(r2lBivTestCorSpearman)
cat(r2lBivTestCorSpearman(y1,y2,out="html"))
cat(r2lBivTestCorSpearman(y2,y3,out="html"))

cleanProg(r2lBivTestStudent)
cat(r2lBivTestStudent(y1,f1,out="html"))

cleanProg(r2lBivTestWilcoxon)
cat(r2lBivTestWilcoxon(y1,f1,out="html"))

cleanProg(r2lBivTestAnova)
cat(r2lBivTestAnova(y1,f2,out="html"))

cleanProg(r2lBivTestKruskalWallis)
cat(r2lBivTestKruskalWallis(y1,f2,out="html"))

cleanProg(r2lBivTestKhi2)
cat(r2lBivTestKhi2(f1,f2,out="html"))
cat(r2lBivTestKhi2(f1,f1b,out="html"))

cleanProg(r2lBivTestOddsRatio)
cat(r2lBivTestOddsRatio(f1,f1b,out="html"))

cleanProg(r2lBivTest)
cat(r2lBivTest(y1,f1,test="Student",line=c(T,F),out="html"))
cat(r2lBivTest(y1,f2,test="Anova",line=c(F,T),out="html"))
cat(r2lBivTest(y1,o1,test=c("Anova","CorPearson"),line=c(T,F,F),out="html"))
cat(r2lBivTest(y1,o1,test=c("Anova","CorPearson"),line=c(T,T,T),out="html"))
cat(r2lBivTest(y1,y2,test="CorPearson",line=c(T,F),out="html"))
cat(r2lBivTest(f1,f1b,test=c("Khi2","OddsRatio"),line=c(F,F,F),out="html"))


###############################################################
###                          graphs                         ###
###############################################################

cleanProg(r2lDensities)
r2lDensities(y=y1,x=f1,main="", xlab="", ylab="")
r2lDensities(y1,f2)
r2lDensities(y2,f2)

####################
#                  #
#   Latex output   #
#                  #
####################

cleanProg(r2lMakePlot)
r2lMakePlot(kind="boxplot",arguments=list(x=y1),type="png",graphName="V1",graphDir="toto", out="latex")
r2lMakePlot(kind="boxplot",arguments=list(y1~f2),type="png",graphName="V2",graphDir="toto", out="latex")

cleanProg(r2lGraphQQPlot)
cat(r2lGraphQQPlot(x=y1,type="png",graphName="V3",graphDir="toto", out="latex"))
cat(r2lGraphQQPlot(x=y2,type="png",graphName="V4",graphDir="toto", out="latex"))

cleanProg(r2lGraphHist)
cat(r2lGraphHist(x=y1,type="png",graphName="V5",graphDir="toto", out="latex"))
cat(r2lGraphHist(x=y2,type="png",graphName="V6",graphDir="toto", out="latex"))

cleanProg(r2lGraphBoxplot)
cat(r2lGraphBoxplot(y1,type="png",graphName="V7",graphDir="toto", out="latex"))
cat(r2lGraphBoxplot(y1,f1,type="png",graphName="V8",graphDir="toto", out="latex"))
cat(r2lGraphBoxplot(y1,f2,type="png",graphName="V9",graphDir="toto", out="latex"))

cleanProg(r2lGraphMosaicPlot)
cat(r2lGraphMosaicPlot(f1,f2,type="png",graphName="V10",graphDir="toto", out="latex"))
cat(r2lGraphMosaicPlot(f2,f1b,type="png",graphName="V11",graphDir="toto", out="latex"))

cleanProg(r2lGraphScatterPlot)
cat(r2lGraphScatterPlot(y1,y2,type="png",graphName="V12",graphDir="toto", out="latex"))
cat(r2lGraphScatterPlot(y2,y3,type="png",graphName="V13",graphDir="toto", out="latex"))

cleanProg(r2lGraphBarplot)
cat(r2lGraphBarplot(f1,type="png",graphName="V14",graphDir="toto", out="latex"))
cat(r2lGraphBarplot(f2,type="png",graphName="V15",graphDir="toto", out="latex"))
cat(r2lGraphBarplot(f2,f1,type="png",graphName="V16",graphDir="toto", out="latex"))

cleanProg(r2lGraphDensity)
cat(r2lGraphDensity(y1,f1,type="png",graphName="V",graphDir="toto", out="latex"))
cat(r2lGraphDensity(y1,f2,type="png",graphName="V",graphDir="toto", out="latex"))
cat(r2lGraphDensity(y2,f2,type="png",graphName="V",graphDir="toto", out="latex"))


###################
#                 #
#   Html output   #
#                 #
###################

cleanProg(r2lMakePlot)
r2lMakePlot(kind="boxplot",arguments=list(x=y1),type="png",graphName="V1",graphDir="toto", out="html")
r2lMakePlot(kind="boxplot",arguments=list(y1~f2),type="png",graphName="V2",graphDir="toto", out="html")

cleanProg(r2lGraphQQPlot)
cat(r2lGraphQQPlot(x=y1,type="png",graphName="V3",graphDir="toto", out="html"))
cat(r2lGraphQQPlot(x=y2,type="png",graphName="V4",graphDir="toto", out="html"))

cleanProg(r2lGraphHist)
cat(r2lGraphHist(x=y1,type="png",graphName="V5",graphDir="toto", out="html"))
cat(r2lGraphHist(x=y2,type="png",graphName="V6",graphDir="toto", out="html"))

cleanProg(r2lGraphBoxplot)
cat(r2lGraphBoxplot(y1,type="png",graphName="V7",graphDir="toto", out="html"))
cat(r2lGraphBoxplot(y1,f1,type="png",graphName="V8",graphDir="toto", out="html"))
cat(r2lGraphBoxplot(y1,f2,type="png",graphName="V9",graphDir="toto", out="html"))

cleanProg(r2lGraphMosaicPlot)
cat(r2lGraphMosaicPlot(f1,f2,type="png",graphName="V10",graphDir="toto", out="html"))
cat(r2lGraphMosaicPlot(f2,f1b,type="png",graphName="V11",graphDir="toto", out="html"))

cleanProg(r2lGraphScatterPlot)
cat(r2lGraphScatterPlot(y1,y2,type="png",graphName="V12",graphDir="toto", out="html"))
cat(r2lGraphScatterPlot(y2,y3,type="png",graphName="V13",graphDir="toto", out="html"))

cleanProg(r2lGraphBarplot)
cat(r2lGraphBarplot(f1,type="png",graphName="V14",graphDir="toto", out="html"))
cat(r2lGraphBarplot(f2,type="png",graphName="V15",graphDir="toto", out="html"))
cat(r2lGraphBarplot(f2,f1,type="png",graphName="V16",graphDir="toto", out="html"))

cleanProg(r2lGraphDensity)
cat(r2lGraphDensity(y1,f1,type="png",graphName="V",graphDir="toto", out="html"))
cat(r2lGraphDensity(y1,f2,type="png",graphName="V",graphDir="toto", out="html"))
cat(r2lGraphDensity(y2,f2,type="png",graphName="V",graphDir="toto", out="html"))



