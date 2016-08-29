library(lattice)
library(TSA)

OLn <- 12
t <- 1:3000
limX <- c(min(t), max(t))
limY <- c(-20, 20)
ts <- rnorm(3000)

ol <- c(128,380,432,584,1436,1388,1640,1892,2484,2396,2548,2700)
ts[ol] <- rnorm(OLn) + c(-6,10,-14,6,-11,14,-6,10,-14,14,-12,6)
#ol <- seq(128, 2900, length.out=12)
#ol <- sample(t,OLn)
#ts[ol] <- rnorm(OLn) + sample(c(seq(-14,-6,4),seq(6,14,4)),OLn, replace=TRUE)

m1=arima(ts,order=c(1,0,0))
dOL <- detectIO(m1)
dOL$ind

lok <- matrix(c(dOL$ind,ts[dOL$ind]),length(ol),2)

koord <- function(ob){
  marY <- limY[2]
  marX <- limX[1]
  if(ts[dOL$ind[ob]] < 0){ marY <- limY[1]}
  if(dOL$ind[ob] > limX[2]/2){ marX <- limX[2]}
  x <- c(marX, dOL$ind[ob],dOL$ind[ob],dOL$ind[ob])
  y <- c(ts[dOL$ind[ob]], ts[dOL$ind[ob]],ts[dOL$ind[ob]], marY)
  lin <- cbind(x,y)
  return(lin)
}

lft <- dOL$ind[dOL$ind<limX[2]/2]
rgt <- dOL$ind[dOL$ind>=limX[2]/2]
top <- dOL$ind[ts[dOL$ind] > 0]
btt <- dOL$ind[ts[dOL$ind] <= 0]

axis.set=function(side,...){
  if(side == "left"){
    panel.axis(side=side, outside=TRUE,
               at=ts[lft],
               labels=format(ts[lft],digits=2, nsmall=1),
               tck=0,
               text.cex=0.7,
               text.col="white")
  }else{
    if(side == "right"){
      panel.axis(side=side, outside=TRUE,
                 at=ts[rgt],
                 labels=format(ts[rgt],digits=2, nsmall=1),
                 tck=0,
                 text.cex=0.7,
                 text.col="white")
    }else{
      if(side == "top"){
        panel.axis(side=side, outside=TRUE, 
                   at=top,
                   labels=paste("Obs#\n",format(top, big.mark=",")),
                   tck=0,
                   text.col="white",
                   text.cex=0.7,
                   rot=0)
      }else{
        if(side == "bottom"){
          panel.axis(side=side, outside=TRUE, 
                     at=btt,
                     labels=paste("Obs#\n",format(btt, big.mark=",")),
                     tck=0,
                     text.col="white",
                     text.cex=0.7,
                     rot=0)
        }
      }
    }
  }}

xyplot(ts~t, aspect=0.3,
       scales=list(axs = "i"),
       par.settings=list(axis.line=list(col="transparent"),
                         background=list(col="black"),
                         axis.components=list(right=list(pad2=5),
                                              left=list(pad2=5))),
       xlab=NULL, ylab=NULL,
       axis=axis.set,
       ylim=c(limY[1],limY[2]),
       panel=function(x,...){
         for(i in 1:OLn){
           panel.lines(koord(i), col="gray", lty=3, alpha=0.4) 
         }
         panel.xyplot(x,...,type="h", col="white", alpha=0.9)
         sizc <- 4 + 6*abs(ts[dOL$ind])/max(abs(ts[dOL$ind]))
         sz <- list(c(1),sizc)
         cl <- c("white","white")
         al <- c(1,0.3)
         ph <- c(16,16)
         for(i in 1:2){
           panel.points(lok, pch=ph[i], cex=sz[[i]], col=cl[i], alpha=al[i])
         }
         panel.abline(h=0, col="black", lwd=2)
       })
