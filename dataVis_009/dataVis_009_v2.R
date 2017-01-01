library(lattice)
library(RColorBrewer)
library(Quandl)
library(emojifont)

data <- Quandl("EPI/40")

countries <- colnames(data)
countries <- countries[2:7] 
countries <- lapply(strsplit(countries,' \\('), function(x) x[1])
countries <- unlist(countries)

Year <- as.POSIXlt(data[,1])
Year <- Year$year+1900

data.long <- reshape(data[,2:7],
                     timevar="Country",
                     times=countries,
                     idvar="Year",
                     ids=Year,
                     direction="long",
                     varying = list(1:6),
                     v.names="NumCig")

data.long <- data.long[order(data.long$Country),]
data.long$Country <- as.factor(data.long$Country)

indMax <- c()
indMin <- c()
for(i in 1:6){
  bool <- data.long$Country == levels(data.long$Country)[i]
  
  tmpInd <- which(data.long[bool,2]==max(data.long[bool,2]))
  tmpInd <- (i-1)*44+tmpInd
  indMax <- c(indMax,tmpInd)
  
  tmpInd <- which(data.long[bool,2]==min(data.long[bool,2]))
  tmpInd <- (i-1)*44+tmpInd
  indMin <- c(indMin,tmpInd)
}

clr <- rep("blue",264)
clr[indMax] <- "red"
clr[indMin] <- "green"

txt.X <- c(data.long[indMin,3],data.long[indMax,3])
txt.Y <- rep(1.5:6.5,2)

txt.lbl.min <- paste("Min:",
                     format(data.long[indMin,2],digits=1,nsmall=0,big.mark=","),
                     " Year:",
                     data.long[indMin,3],
                     sep=" ")

txt.lbl.max <- paste("Max:",
                     format(data.long[indMax,2],digits=1,nsmall=0,big.mark=","),
                     " Year:",
                     data.long[indMax,3],
                     sep=" ")

txt.lbl <- c(txt.lbl.min, txt.lbl.max)


bp <- brewer.pal(11,"Spectral")[c(1:4,8:11)]
crp <- colorRampPalette(rev(bp))(length(data.long$NumCig))
crp <- crp[1:230]

ky <- rev(round(seq(min(data.long$NumCig),max(data.long$NumCig), length.out=16)))
ky.min <- as.character(round(min(data.long$NumCig)))
ky.max <- as.character(round(max(data.long$NumCig)))

axis.set=function(side,...){
  if(side == "left"){
    panel.axis(side=side, outside=TRUE, at=1:6,
               labels=levels(data.long$Country),
               text.col="gray35",
               text.cex = 1.5)
  }else{
    if(side == "bottom"){
      panel.axis(side=side, outside=TRUE,
                 at=round(seq(1960,2003, length.out=7)),
                 text.col="gray35",
                 text.cex = 1.5)
      
    }else axis.default(side=side, ...)
  }
}

d <- round(max(data.long$NumCig)-min(data.long$NumCig)+1)/length(crp)
cl.ind <- trunc((data.long$NumCig-round(min(data.long$NumCig)))/d+1)

cigcon <- dotplot(Country~Year,data=data.long,
              xlab=NULL, main="Cigarette Consumption Per Person by Country, 1960-2003",
              par.settings=list(dot.line=list(col="transparent"),
                                axis.line=list(col="transparent"),
                                par.main.text=list(col="gray45", cex = 2),
                                axis.components=list(bottom=list(pad1=0),
                                                     left=list(pad1=-5, pad2=5))),
              
              axis=axis.set,
              
              panel=function(...){
                panel.text(..., alpha=1,
                           labels=emoji("smoking"),
                           fontfamily='OpenSansEmoji', srt = 0,
                           col=crp[cl.ind],
                           cex=data.long$NumCig/650)
                
                cl <- c(crp[cl.ind[indMin]],crp[cl.ind[indMax]])
                cl.txt <- "gray25"
                panel.text(txt.X, txt.Y, txt.lbl, cex=1.3, col=cl.txt)
                panel.arrows(txt.X,rep(1:6,2),txt.X,rep(1.35:6.35,2), code=2, 
                             length=0.30, angle=90, col=cl, lty=1, alpha=1)
              })

png("dataVis_009_v2.png", width=10, height=4, units="in", res=300)
cigcon
dev.off()