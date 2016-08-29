library(lattice)
library(RColorBrewer)
library(Quandl)

data <- Quandl("FBI/CRIME11")

ind <- seq(1,19,2)
data.rate <- data[ind]

pt <- data.rate[,2:10]/unlist(rep(data.rate[20,2:10], each=20))

pt.m <- as.matrix(pt)
pt.m <- pt.m[sort(index(pt.m), decreasing=TRUE),]

year <- as.POSIXlt(data$Year)
year <- sort(year$year+1900)

pop <- data$Population
pop <- pop[order(index(pop), decreasing=TRUE)]
pop.format <- paste("US pop.\n",round(pop,-6)/1000000,"M",sep=" ")
pop.tck <- ((pop/100000000)-1.8)*6
pop.tck

colRmp <- colorRampPalette(brewer.pal(11,"RdBu"))(100)
colRmp <- colRmp[order(index(colRmp), decreasing=TRUE)]

axis.set=function(side,...){
  if(side == "left"){
    panel.axis(side=side, outside=TRUE, at=1:9,
               tck=0,
               text.col=colRmp[100],
               labels=colnames(pt.m))
  }else{
    if(side == "bottom"){
      panel.axis(side=side, outside=TRUE, at=1:20,
                 tck=pop.tck,
                 line.col=colRmp[100],
                 line.lwd=2,
                 text.col="transparent",
                 text.cex=0.8,
                 rot=0)
      panel.axis(side=side, outside=TRUE, at=1:20,
                 tck=3.5,
                 line.col="white",
                 line.lwd=2,
                 text.col=colRmp[1],
                 labels=pop.format,
                 text.cex=0.8,
                 rot=0)
    }else{
      if(side == "top"){
        panel.axis(side=side, outside=TRUE, at=1:20,
                   tck=0,
                   line.col="white",
                   text.col=c(colRmp[100],rep(colRmp[1],19)),
                   labels=year,
                   text.cex=c(1.2,rep(0.8,19)),
                   rot=0)
        panel.axis(side=side, outside=TRUE, at=1,
                   tck=3.5,
                   line.col="transparent",
                   text.col=colRmp[100],
                   labels="Reference\nYear",
                   rot=0)
      }else axis.default(side=side, ...)
    }
  }
}


levelplot(pt.m, aspect="iso", shrink = c(0.8, 0.8),
          xlab=NULL, ylab=NULL,
          axis=axis.set,
          par.settings=list(axis.line=list(col="transparent"),
                            axis.components=list(bottom=list(pad1=-4.5, 
                                                             pad2=10),
                                                 right=list(pad2=10),
                                                 left=list(pad2=10),
                                                 top=list(pad2=10))),
          col.regions=colRmp,
          colorkey = list(space="top")
)

