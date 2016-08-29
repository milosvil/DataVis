library(lattice)
library(RColorBrewer)

ser <- 1000
obs <- 100
m <- 0
sd <- seq(1.2,3, length.out=ser)
xax <- seq(-10,10,length.out=obs)
Ndists <- matrix(rep(0,ser*obs), obs,ser)

for(i in 1:ser){
  Ndists[,i] <- dnorm(xax,m,sd[i])
}
Ndists.df <- as.data.frame(Ndists)

Ndists.long <- reshape(Ndists.df, 
                       direction="long",
                       varying = list(1:ser),
                       v.names="dist")

Ndists.long$xax <- rep(xax,ser)

axis.set <- function(side, ...) {
  if (side == "left" | side == "top" | side=="bottom" | side=="right") {
    panel.axis(side = side, outside = TRUE,
               tick=FALSE,
               labels=FALSE)
  }else axis.default(side = side, ...)
}

xyplot(dist~xax, groups=time, data=Ndists.long, 
       type=c("l"), aspect=1, 
       xlab=NULL, ylab=NULL, 
       axis=axis.set,
       
       col=colorRampPalette(brewer.pal(11,"BrBG"))(ser),
       par.settings=list(axis.line=list(col="transparent")),
       
       key=list(space="right",
                title="Standard\n deviation",
                cex.title=0.9,
                columns=1,
                cex=0.8,
                adj=1,
                text=list(as.character(format(round(seq(1.2,3.0, length.out=11),1),nsmall=1)),
                          col=brewer.pal(11,"BrBG")),
                points=list(pch=15, cex=2, col=brewer.pal(11,"BrBG"))))

