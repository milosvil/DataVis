library(lattice)
library(latticeExtra)
library(RColorBrewer)

obs <- 10000
pnl <- 32

x <- rnorm(obs)
x <- rep(x,pnl)
y <- rnorm(obs)
y <- rep(y,pnl)
fac <- seq(1,pnl,length.out=pnl)
fac <- rep(fac, each=obs)

df <- data.frame(fac,x,y)
df$y1 <- rep(seq(-5,5,length.out=pnl),each=obs)*df$x + rnorm(obs*pnl,0,1.5)
df$fac <- as.factor(df$fac)

cocor <- NULL
for(i in 1:pnl){
  kor <- cor(df[(obs*(i-1)+1):(obs*(i-1)+obs),2],df[(obs*(i-1)+1):(obs*(i-1)+obs),4])
  cocor <- c(cocor,kor)
}

corel <- xyplot(y1~x|fac, data=df, aspect="iso", 
                scale = list(draw=FALSE, x="same", y="same"), 
                ylab=NULL, xlab=NULL, strip=FALSE, layout=c(pnl,1),
                par.settings=list(axis.line=list(col="transparent")),
                
                key=list(space="top",
                         title="Correlation coefficient \n",
                         cex.title=0.8,
                         columns=8,
                         cex=0.7,
                         adj=1,
                         text=list(as.character(format(round(cocor,2),nsmall=1)), col="gray65"),
                         points=list(pch=15, cex=1, col=colorRampPalette(brewer.pal(11,"Spectral"))(pnl))),
                
                panel=function(..., subscripts){
                  pn <- panel.number()
                  clr <- colorRampPalette(brewer.pal(11,"Spectral"))(pnl)
                  panel.xyplot(..., col=clr[pn], pch=".",alpha=0.4, cex=1)
                })

plot(corel)
