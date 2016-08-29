library(lattice)

data(USArrests)
USArrests$State <- rownames(USArrests)
rownames(USArrests) <- 1:50

axis.set <- function(side, ...) {
  if (side == "left") {
    panel.axis(side = side, outside = TRUE, at=1:50, 
               tck = sort(USArrests$UrbanPop)/10,
               line.col = colorRampPalette(c("green","purple"))(50),
               line.lwd= 2,
               text.col = colorRampPalette(c("green","purple"))(50), 
               text.cex=0.8,
               labels=paste(as.character(
                 sort(
                   reorder(USArrests$State, USArrests$UrbanPop))),
                 as.character(sort(USArrests$UrbanPop)), sep="  "))
  }else{
    if (side == "bottom") {
      panel.axis(side = side, outside = TRUE, at=c(0,10,20,30,40), 
                 tck=0,
                 text.col = "green",
                 text.cex= 0.8, 
                 rot=0)
    }else axis.default(side = side, ...)
  }
  
}

orUP <- order(USArrests$UrbanPop)
df <- data.frame(orUP, cr=colorRampPalette(c("green","purple"))(50))
df <- df[order(df$orUP),]

dotplot(reorder(State,UrbanPop) ~ Rape+Murder, data=USArrests, 
        origin=0, aspect=1.5,
        xlab=NULL,
        axis=axis.set,
        
        key=list(space = "top",
                 text = list(c("Rape arrests","Murder arrests")),
                 columns = 2,
                 col = "gray35",
                 cex = 0.8,
                 points = list(pch=c(16,1), col="gray35")),
        
        par.settings=list(dot.line=list(col=as.character(df$cr), lwd=1),
                          panel.background=list(col="black"),
                          background=list(col="black"),
                          axis.components=list(left=list(pad1=1, pad2=12)),
                          axis.line=list(col="transparent")),
        
        panel=function(x,y,...){
          panel.dotplot(x,y,...,pch=list(c(16),c(1)), cex=1, 
                        col=list(c(as.character(df$cr)),c(as.character(df$cr))))
        })