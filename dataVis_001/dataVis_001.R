library(lattice)

data <- c()
stdev <- c(1:10,9:1)

for(i in 1:19){
  data <- c(data, rnorm(2000,0,stdev[i]))
}

fac <- gl(19,2000)
df <- data.frame(fac, data)

dotplot(fac~data, data=df, 
        jitter.y=TRUE, jitter.x=TRUE, 
        alpha=0.2, aspect=0.4,
        par.settings=list(dot.line=list(col="transparent"),
                          background=list(col="black")),
        
        panel=function(...){
          clr <- rep("#0080ff", 38000)
          Fac <- as.character(levels(fac))
          for(i in 1:19){
            StDev <- sd(df[fac==Fac[i],2])
            clr[fac==Fac[i] & data >= 1.96*StDev] <- "green"
            clr[fac==Fac[i] & data <= -1.96*StDev] <- "green"
          }
          panel.dotplot(..., col=clr, pch=16)
        })
