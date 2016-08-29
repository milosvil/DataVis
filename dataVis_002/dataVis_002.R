library(lattice)

nLines <- 500
interval <- 2000
t <- 1:interval
mat <- matrix(rep(0,nLines*interval), interval,nLines)
for(i in 1:nLines){
  mat[2:interval,i] <- cumsum(rnorm(interval-1))
}

df <- data.frame(t, mat)

xyplot(X1 ~ t, data=df, 
       type="n", ylim=c(min(mat), max(mat)), 
       xlab=NULL, ylab=NULL,
       scale=list(draw=FALSE, x=list(relation="same", axs="i")),
       par.settings = list(background=list(col="black"),
                           axis.line=list(col="transparent"),
                           axis.components=list(right=list(tck=0,pad1=0,pad2=0),
                                                left=list(tck=0,pad1=0,pad2=0))),
       panel=function(...){
         panel.xyplot(...)
         cr <- colorRampPalette(c("black","purple"))(nLines)
         for(i in 2:nLines+1){
           panel.lines(df[,i]~df$t, type="l", 
                       col=cr[i-1], alpha=0.7) }
       })
