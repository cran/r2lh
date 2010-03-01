#source("./testFunctions.R")
source("../R/numeric.R")



####################
#                  #
#   Latex output   #
#                  #
####################

# Type Postscript

sink(file="bivContLogical.tex")
r2lBivContinuousLogical(y1,f1,graphDir="graphBiv",graphName="V1",out="latex")
sink()

sink(file="bivContFactor.tex")
r2lBivContinuousFactorWide(y1,f1,graphDir="graphBiv",graphName="V3a",out="latex")
#r2lBivContinuousFactorMixed(y1,f2,graphDir="graphBiv",graphName="V3b",out="latex")
r2lBivContinuousFactorLong(y1,f3,graphDir="graphBiv",graphName="V3c",out="latex")
sink()

#r2lBivContinuousFactor(y1,f2,graphDir="graphBiv",graphName="V3",out="latex",displayStyle=3)
#r2lBivContinuousFactor(y1,f3,graphDir="graphBiv",graphName="V3",out="latex",displayStyle=10)


sink(file="bivContOrdered.tex")
r2lBivContinuousOrderedWide(y2,o1,graphDir="graphBiv",graphName="V4",out="latex")
r2lBivContinuousOrderedMixed(y2,o2,graphDir="graphBiv",graphName="V5",out="latex")
r2lBivContinuousOrderedLong(y2,o3,graphDir="graphBiv",graphName="V6",out="latex")
sink()

#r2lBivContinuousOrdered(y2,o1,graphDir="graphBiv",graphName="V4",out="latex")
#r2lBivContinuousOrdered(y2,o2,graphDir="graphBiv",graphName="V5",out="latex")
#r2lBivContinuousOrdered(y2,o3,graphDir="graphBiv",graphName="V6",out="latex")


sink(file="bivContDiscrete.tex")
r2lBivContinuousDiscreteWide(y1,d1,graphDir="graphBiv",graphName="V7",out="latex")
r2lBivContinuousDiscreteMixed(y2,d2,graphDir="graphBiv",graphName="V8",out="latex")
r2lBivContinuousDiscreteLong(y3,d3,graphDir="graphBiv",graphName="V9",out="latex")
sink()

#r2lBivContinuousDiscrete(y1,d1,graphDir="graphBiv",graphName="V7",out="latex")
#r2lBivContinuousDiscrete(y2,d2,graphDir="graphBiv",graphName="V8",out="latex")
#r2lBivContinuousDiscrete(y3,d3,graphDir="graphBiv",graphName="V9",out="latex")

sink(file="bivContContinuous.tex")
r2lBivContinuousContinuous(y1,y2,graphDir="graphBiv",graphName="V10",out="latex")
sink()


###################
#                 #
#   Html output   #
#                 #
###################

# Type PNG
sink(file="bivContLogical.html")
r2lBivContinuousLogical(y1,f1,graphDir="graphBiv",graphName="V1",type="png",out="html")
sink()

sink(file="bivContFactor.html")
r2lBivContinuousFactorWide(y1,f2,graphDir="graphBiv",graphName="V2",type="png",out="html")
r2lBivContinuousFactorLong(y1,f3,graphDir="graphBiv",graphName="V3",type="png",out="html")
sink()


#r2lBivContinuousFactor(y1,f1,graphDir="graphBiv",graphName="V3",type="png",out="html")
#r2lBivContinuousFactor(y1,f2,graphDir="graphBiv",graphName="V3",type="png",out="html")
#r2lBivContinuousFactor(y1,f3,graphDir="graphBiv",graphName="V3",type="png",out="html")

#r2lBivContinuousFactor(y1,f2,graphDir="graphBiv",graphName="V3",type="png",out="html",displayStyle=3)
#r2lBivContinuousFactor(y1,f3,graphDir="graphBiv",graphName="V3",type="png",out="html",displayStyle=10)

sink(file="bivContOrdered.html")
r2lBivContinuousOrderedWide(y2,o1,graphDir="graphBiv",graphName="V4",type="png",out="html")
r2lBivContinuousOrderedMixed(y2,o2,graphDir="graphBiv",graphName="V5",type="png",out="html")
r2lBivContinuousOrderedLong(y2,o3,graphDir="graphBiv",graphName="V6",type="png",out="html")
sink()

#r2lBivContinuousOrdered(y2,o1,graphDir="graphBiv",graphName="V4",type="png",out="html")
#r2lBivContinuousOrdered(y2,o2,graphDir="graphBiv",graphName="V5",type="png",out="html")
#r2lBivContinuousOrdered(y2,o3,graphDir="graphBiv",graphName="V6",type="png",out="html")

sink(file="bivContDiscrete.html")
r2lBivContinuousDiscreteWide(y1,d1,graphDir="graphBiv",graphName="V7",type="png",out="html")
r2lBivContinuousDiscreteMixed(y2,d2,graphDir="graphBiv",graphName="V8",type="png",out="html")
r2lBivContinuousDiscreteLong(y3,d3,graphDir="graphBiv",graphName="V9",type="png",out="html")
sink()

#r2lBivContinuousDiscrete(y1,d1,graphDir="graphBiv",graphName="V7",type="png",out="html")
#r2lBivContinuousDiscrete(y2,d2,graphDir="graphBiv",graphName="V8",type="png",out="html")
#r2lBivContinuousDiscrete(y3,d3,graphDir="graphBiv",graphName="V9",type="png",out="html")

sink(file="bivContContinuous.html")
r2lBivContinuousContinuous(y1,y2,graphDir="graphBiv",graphName="V10",type="png",out="html")
sink()

