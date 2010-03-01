source("./testFunctions.R")
source("../R/numeric.R")
source("../R/rtlb.R")



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

