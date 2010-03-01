source("./testFunctions.R")
source("../R/numeric.R")


######################################################################
###                           Dispatching                          ###
###                               S4                               ###
######################################################################

setClass(Class="continuous",contains=c("numeric"))
setClass(Class="discrete",contains=c("numeric"))

setGeneric(name="r2lBiv",def=function(y,x,textBefore="",textAfter="",graphDir="graphBiv",graphName="V",type="png",out="latex",displayStyle="wide"){standardGeneric("r2lBiv")})

for(i in 1:5){
    for(j in 1:5){
        cat(paste("f",i,j,"<- function(y,x,textBefore='',textAfter='',graphDir='graphBiv',
graphName='V',type='png',out='latex',displayStyle='wide')cat('f",i,"-",j,"; Y=',class(y),' X=',class(x),'Display=',displayStyle,'\\n')\n",sep=""))
    }
}

setMethod(f="r2lBiv",signature=c("logical","logical"),def=f11)#r2lBivContinuousLogical)
setMethod(f="r2lBiv",signature=c("logical","factor"),def=f12)#r2lBivContinuousFactor)
setMethod(f="r2lBiv",signature=c("logical","ordered"),def=f13)#r2lBivContinuousOrdered)
setMethod(f="r2lBiv",signature=c("logical","discrete"),def=f14)#r2lBivContinuousDiscrete)
setMethod(f="r2lBiv",signature=c("logical","continuous"),def=f15)#r2lBivContinuousContinuous)

setMethod(f="r2lBiv",signature=c("factor","logical"),def=f21)#r2lBivContinuousLogical)
setMethod(f="r2lBiv",signature=c("factor","factor"),def=f22)#r2lBivContinuousFactor)
setMethod(f="r2lBiv",signature=c("factor","ordered"),def=f23)#r2lBivContinuousOrdered)
setMethod(f="r2lBiv",signature=c("factor","discrete"),def=f24)#r2lBivContinuousDiscrete)
setMethod(f="r2lBiv",signature=c("factor","continuous"),def=f25)#r2lBivContinuousContinuous)

setMethod(f="r2lBiv",signature=c("ordered","logical"),def=f31)#r2lBivContinuousLogical)
setMethod(f="r2lBiv",signature=c("ordered","factor"),def=f32)#r2lBivContinuousFactor)
setMethod(f="r2lBiv",signature=c("ordered","ordered"),def=f33)#r2lBivContinuousOrdered)
setMethod(f="r2lBiv",signature=c("ordered","discrete"),def=f34)#r2lBivContinuousDiscrete)
setMethod(f="r2lBiv",signature=c("ordered","continuous"),def=f35)#r2lBivContinuousContinuous)

setMethod(f="r2lBiv",signature=c("discrete","logical"),def=f41)#r2lBivContinuousLogical)
setMethod(f="r2lBiv",signature=c("discrete","factor"),def=f42)#r2lBivContinuousFactor)
setMethod(f="r2lBiv",signature=c("discrete","ordered"),def=f43)#r2lBivContinuousOrdered)
setMethod(f="r2lBiv",signature=c("discrete","discrete"),def=f44)#r2lBivContinuousDiscrete)
setMethod(f="r2lBiv",signature=c("discrete","continuous"),def=f45)#r2lBivContinuousContinuous)

setMethod(f="r2lBiv",signature=c("continuous","logical"),def=f51)#r2lBivContinuousLogical)
setMethod(f="r2lBiv",signature=c("continuous","factor"),def=f52)#r2lBivContinuousFactor)
setMethod(f="r2lBiv",signature=c("continuous","ordered"),def=f53)#r2lBivContinuousOrdered)
setMethod(f="r2lBiv",signature=c("continuous","discrete"),def=f54)#r2lBivContinuousDiscrete)
setMethod(f="r2lBiv",signature=c("continuous","continuous"),def=f55)#r2lBivContinuousContinuous)



changeClass <- function(x,limDiscreteX){
    if(class(x)[1]=="character"){x <- as.factor(x)}else{}
    if(length(table(x))==2){class(x) <- "logical"}
    if(class(x)[1] %in% c("numeric","integer")){
        if(length(table(x))<=limDiscreteX) {
            class(x) <- c("discrete","numeric")
        }else{
            class(x) <- c("continuous","numeric")
        }
    }else{}
    return(x)
}


################################
### Function rtlb, highest level.
### It open a file, create the direcetories, cut the formula in two variable then call r2lBiv

rtlb <- function(formule,fileOut="",textBefore="",textAfter="",graphDir="graphBiv",graphName="V",type="png",displayStyle=7,limDiscreteY=10,limDiscreteX=10){
    if (fileOut!="") {
        on.exit(sink())
        sink(fileOut)
    }
    if (graphDir!="") {
        dir.create(graphDir,showWarnings=FALSE)
    }
    y <- eval(formule[[2]],envir=parent.frame(n=1))
    x <- eval(formule[[3]],envir=parent.frame(n=1))

    rtlBiv(y,x,textBefore=textBefore,textAfter=textAfter,graphDir=graphDir,graphName=graphName,type=type,out="latex",displayStyle=displayStyle,limDiscreteY=limDiscreteY,limDiscreteX=limDiscreteX)
    return(invisible())
}

rtlBiv <- function(y,x,textBefore="",textAfter="",graphDir="graphBiv",graphName="V",type="png",out="latex",displayStyle=7,limDiscreteY=10,limDiscreteX=10){
    if(class(x)[1]=="data.frame"){
        cat(r2lComment("r2lu.data.frame",out=out))
        nbVar <- length(x)
        if (length(textBefore)==1) {textBefore <- rep(textBefore,time=nbVar)}else{}
        if (length(textAfter)==1) {textAfter <- rep(textAfter,time=nbVar)}else{}
        if (length(graphName)==1) {graphName <- paste(graphName,1:nbVar,sep="")}else{}
        if (length(limDiscreteX)==1) {limDiscreteX <- rep(limDiscreteX,time=nbVar)}else{}
        if (length(displayStyle)==1) {displayStyle <- rep(displayStyle,time=nbVar)}else{}

        if (class(displayStyle)=="list") {
            if(is.null(displayStyle$lim)){
                displayAux <- as.character(rep(7,time=nbVar))
            }else{
                displayAux <- as.character(rep(displayStyle$lim,time=nbVar))
            }

            displayAux[names(x)%in%displayStyle$wide] <- "wide"
            displayAux[names(x)%in%displayStyle$long] <- "long"
            displayStyle <- displayAux
        }
        for (i in 1:nbVar) {
            rtlBiv(y,x[,i],textBefore=textBefore[i],textAfter=textAfter[i],graphDir=graphDir,graphName=graphName[i],
                   type=type,out="latex",displayStyle=displayStyle[i],limDiscreteY=limDiscreteY,limDiscreteX=limDiscreteX[i])
        }
    }else{
        y <- changeClass(y,limDiscreteY)
        x <- changeClass(x,limDiscreteX)
        if(!displayStyle%in%c("wide","long")){
            if(as.integer(displayStyle)>=length(table(x))){
                displayStyle <- "wide"
            }else{
                displayStyle <- "long"
            }
        }
        r2lBiv(y=y,x=x,textBefore=textBefore,textAfter=textAfter,graphDir=graphDir,graphName=graphName,
               type=type,out="latex",displayStyle=displayStyle)
    }
    return(invisible())
}





####################
#                  #
#   Latex output   #
#                  #
####################

ds <- list(lim=4,wide=c("f1","o2"),long=c("f2","y2"))
ds2 <- list(wide=c("f1","o2"),long=c("f2","y2"))
### Logical
rtlb(f1~f1)
rtlb(f1~f2)
rtlb(f1~f3)

rtlb(f1~o1)
rtlb(f1~o2)
rtlb(f1~o3)

rtlb(f1~i1)
rtlb(f1~i2)
rtlb(f1~i3)
rtlb(f1~i2,limDiscreteX=3)
rtlb(f1~df,displayStyle=ds)

rtlb(f1~y2)
rtlb(f1~y3)

rtlb(f1~df)


### Factor
rtlb(f2~f1)
rtlb(f2~f2)
rtlb(f2~f3)

rtlb(f2~o1)
rtlb(f2~o2)
rtlb(f2~o3)

rtlb(f2~i1)
rtlb(f2~i2)
rtlb(f2~i3)
rtlb(f2~i2,limDiscreteX=3)
rtlb(f2~i2,displayStyle=3)

rtlb(f2~y2)
rtlb(f2~y3)

rtlb(f2~df)


### Ordered
rtlb(o2~f1)
rtlb(o2~f2)
rtlb(o2~f3)

rtlb(o2~o1)
rtlb(o2~o2)
rtlb(o2~o3)

rtlb(o2~i1)
rtlb(o2~i2)
rtlb(o2~i3)
rtlb(o2~i2,limDiscreteX=3)
rtlb(o2~i2,displayStyle=3)

rtlb(o2~y2)
rtlb(o2~y3)

rtlb(o2~df)


### Discrete
rtlb(i2~f1)
rtlb(i2~f2)
rtlb(i2~f3)

rtlb(i2~o1)
rtlb(i2~o2)
rtlb(i2~o3)

rtlb(i2~i1)
rtlb(i2~i2)
rtlb(i2~i3)
rtlb(i2~i2,limDiscreteX=3)
rtlb(i2~i2,displayStyle=3)

rtlb(i2~y2)
rtlb(i2~y3)

rtlb(i2~df)


### Continuous
rtlb(y1~f1)
rtlb(y1~f2)
rtlb(y1~f3)

rtlb(y1~o1)
rtlb(y1~o2)
rtlb(y1~o3)

rtlb(y1~i1)
rtlb(y1~i2)
rtlb(y1~i3)
rtlb(y1~i2,limDiscreteX=3)
rtlb(y1~i2,displayStyle=3)

rtlb(y1~y2)
rtlb(y1~y3)

rtlb(y1~df)

