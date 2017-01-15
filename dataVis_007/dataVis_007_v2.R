library(lattice)
library(RColorBrewer)
library(Quandl)
library(emojifont)

data <- Quandl("FBI/CRIME11")
str(data)

ind <- c(1,seq(4,20,2))
data.rate <- data[ind]
str(data.rate)

pt <- data.rate[,2:10]/unlist(rep(data.rate[18,2:10], each=18))

pt.m <- as.matrix(pt)
pt.m <- pt.m[sort(index(pt.m), decreasing=TRUE),]

year <- as.POSIXlt(data$Year)
year <- sort(year$year+1900)

pop <- data$Population
pop <- pop[order(index(pop), decreasing=TRUE)]

load.fontawesome()
list.emojifonts()

pop.format <- paste("US pop.\n",round(pop,-6)/1000000,"M",sep=" ")
pop.tck <- ((pop/100000000)-1.8)*6

colRmp <- colorRampPalette(c("#CD0000", "#EE7600", "#008B00"))(162)

colRmp <- colorRampPalette(brewer.pal(11,"RdBu"))(162)
colRmp <- colRmp[order(index(colRmp), decreasing=TRUE)]
colRmp_2 <- colRmp

pt.m.df <- as.data.frame(pt.m)
colRmp <- as.character(sapply(pt.m.df, function(x){ colRmp[round(161*((x-min(x))/(max(x)-min(x)))+1)] }))

bgcol = "gray50"
axiscol = "gray10"

axis.set=function(side,...){
  if(side == "left"){
    panel.axis(side = side, outside = TRUE, at = 1:9,
               tck = 0,
               text.col = axiscol,
               labels = colnames(pt.m))
  }else{
    if(side == "bottom"){
      panel.axis(side=side, outside=TRUE, at=1:18,
                 tck=pop.tck,
                 line.col=axiscol,
                 line.lwd=2,
                 text.col="transparent",
                 text.cex=0.8,
                 rot=0)
      panel.axis(side = side, outside = TRUE, at = 1:18,
                 tck = 3.5,
                 line.col = bgcol,
                 line.lwd = 2,
                 text.col= axiscol,
                 labels = pop.format,
                 text.cex = 0.8,
                 rot = 0)
    }else{
      if(side == "top"){
        panel.axis(side=side, outside=TRUE, at=1:18,
                   tck=0,
                   text.col = axiscol,
                   labels=year,
                   text.cex=c(1.2,rep(0.8,19)),
                   rot=0)
        panel.axis(side=side, outside=TRUE, at=1,
                   tck=3.5,
                   line.col="transparent",
                   text.col=axiscol,
                   labels="Reference\nYear",
                   rot=0)
      }else axis.default(side=side, ...)
    }
  }
}

load.emojifont()
list.emojifonts()

crime_rate <- levelplot(pt.m, aspect="iso", shrink = c(0.8, 0.8),
                        xlab=NULL, ylab=NULL,
                        main = "FBI | U.S. Crime Rate | 1996 - 2013",
                        axis=axis.set,
                        par.settings=list(axis.line=list(col="transparent"),
                                          background=list(col=bgcol),
                                          axis.components=list(bottom=list(pad1=-4.5, 
                                                                           pad2=1),
                                                               right=list(pad2=5),
                                                               left=list(pad2=2),
                                                               top=list(pad2=10))),
                        col.regions=colRmp_2,
                        colorkey = list(space="right", 
                                        labels = list(at=c(0.4, 0.7, 1), 
                                                      labels = c("LOW","MEDIUM","HIGH"), 
                                                      col=axiscol)),
                        panel = function(...){
                         panel.text(..., alpha = 1,
                                    col=colRmp,
                                    labels=emoji("gun"),
                                    fontfamily='OpenSansEmoji', srt = 0,
                                    cex = 2)
                        }
)

png("dataVis_007_v2.png", width=1300, height=750, units="px")
crime_rate
dev.off()