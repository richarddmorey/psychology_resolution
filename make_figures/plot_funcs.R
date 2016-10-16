make.pvalue.plot <- function( r.o, n.o, r.r, n.r, true.d=.3, alpha=0.05, xlims=c(-1,3), par.list = list(), null = TRUE, null.fill = TRUE)
{
  
  do.call(par,par.list)


  zz = seq(xlims[1]-.25,xlims[2]+.25,len=400)

  
  z.d = atanh(r.o) - atanh(r.r)
  sd.d = sqrt(1/(n.o-3) + 1/(n.r-3))
  
  
  y.expand = 1.1
  
  yy = dnorm(zz,true.d,sd.d)
  plot(zz, yy, ty='l', ylab="Density", xlab="Difference (Fisher's z)",axes=FALSE,ylim=c(0,y.expand*max(yy)),lwd=2
       ,xlim=xlims, col="red",lty=2)
  box()
  axis(1)
  axis(3, at = seq(-2,2,.5), lab=round(tanh(seq(-2,2,.5)),2))
  mtext("Difference (correlation, original r=0)",3,2.5,adj=.5)
  
  crit = abs(z.d)
  abline(v=c(-crit,crit),col="gray")
  
  ## Left side
  xx = seq(-crit,crit,len=400)
  yy2 = dnorm(xx,true.d,sd.d)
  polygon(c(xx,rev(xx)),c(yy2,yy2*0),col=rgb(1,0,0,.3),border=NA)
  
  text(-crit,par()$usr[4],substitute(d[z]==r0,list(r0=round(-crit,2))),
       srt=90,adj=c(1.1,-.2),col="gray")
  
  text(crit,par()$usr[4],substitute(d[z]==r0,list(r0=round(crit,2))),
       srt=90,adj=c(1.1,1.2),col="gray")
  
  if(null){
    yy2 = dnorm(zz,0,sd.d)
    lines(zz, yy2, lty=1,lwd=2)
  }
  
  if(null.fill){
    xx = seq(crit,max(zz),len=100)
    yy2 = dnorm(xx,0,sd.d)
    polygon(c(xx,rev(xx)),c(yy2,yy2*0),col=rgb(0,0,0,.3))

    xx = seq(min(zz),-crit, len=100)
    yy2 = dnorm(xx,0,sd.d)
    polygon(c(xx,rev(xx)),c(yy2,yy2*0),col=rgb(0,0,0,.3))
  }
  
}  

make.pvalue.curve.plot <- function( r.o, n.o, r.r, n.r, true.d=.3, xmax=2.5, par.list = list())
{
  do.call(par,par.list)
  dd = seq(0,xmax,len=100)
  
  z.d = atanh(r.o) - atanh(r.r)
  sd.d = sqrt(1/(n.o-3) + 1/(n.r-3))
  
  z.score = z.d/sd.d
  
  pp = pchisq(z.score^2,1,ncp = (dd/sd.d)^2)

  
  plot(dd,pp,ty='l', ylim=c(0,1),
       ylab=expression(paste(p[phantom()>phantom()], " value ")),
       xlab="True difference (abs. value of Fisher's z)",
       col="black", lwd=2, lty=1)
  
  axis(3, at=seq(0,2.5,.33), lab=round(tanh(seq(0,2.5,.33)),2),cex.axis=.75)
  mtext(expression(paste("True difference (correlation, original ", rho==0,")")),
        3,2.5,adj=.5,cex=.83)
  
  segments(true.d,0,true.d,
           pchisq(z.score^2,1,ncp = (true.d/sd.d)^2),col="red")
  
  segments(0,pchisq(z.score^2,1,ncp = (true.d/sd.d)^2),dev,
           pchisq(z.score^2,1,ncp = (true.d/sd.d)^2),col="red")
  
  points(dev,pchisq(z.score^2,1,ncp = (true.d/sd.d)^2),pch=19,col="red")
  
  abline(v=0,col="gray")
  
}