library(lattice)
library(latticeExtra)

x <- seq(-2.5,2.5,length.out=40)
x1 <- dnorm(x,0,1)
x2 <- dnorm(x,0,1)

mat <- x1 %o% x2

colRmp=colorRampPalette(c("black","green"))(100)

wf <- wireframe(mat, xlab=NULL, ylab=NULL, zlab=NULL,
                zoom=1, 
                aspect=1,
                #distance=0,
                #shade=TRUE,
                #row.values = rep(0.3,20),
                #light.source = c(10,0,10),
                screen = list(z = 0, x = 0, y = 0),
                col=colRmp[70],
                col.regions=colRmp,
                #at = do.breaks(range(mat),72),
                #alpha.regions=c(rep(1,1),rep(0,1)),
                #region=FALSE, 
                drape=TRUE,
                colorkey=FALSE,
                
                par.settings=list(axis.line=list(col="transparent"),
                                  box.3d=list(col="transparent"),
                                  background=list(col="white"))
)

nrow <- 3
ncol <- 3
rotx <- seq(-20,-50, length.out=nrow*ncol)
roty <- seq(10,20, length.out=nrow*ncol)
rotz <- seq(145,165, length.out=nrow*ncol)

update(wf[rep(1,nrow*ncol)], layout=c(nrow,ncol),
       panel=function(...,screen){
         pn <- panel.number()
         panel.wireframe(..., screen = list(x = rotx[pn],
                                            y = roty[pn],
                                            z = 10))
       })
