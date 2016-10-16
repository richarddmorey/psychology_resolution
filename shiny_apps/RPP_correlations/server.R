#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source('settings.R')
source('loadData.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  observe({
    # Initially will be empty
    if (is.null(input$myClick)){
      return()
    }
    
    x = input$myClick$x
    y = input$myClick$y
    x1 = atanh(pi_dat_nona$cor_orig)
    y1 = atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep)
  
    d = which.min((x - x1)^2 + (y - y1)^2)
    updateSelectInput( session, "study", selected = d )
    
        
  })
  
  output$table <- renderTable({
    which.pt = as.integer(input$study)
    pi_dat_nona$`Diff. (z)` = atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep)
    
    df = pi_dat_nona[,c("lab", "cor_orig", "n_orig", "cor_rep", "n_rep",
                        "Diff. (z)","se_total")]
    
    colnames(df)=c("Study","Orig. r","Orig. N","Rep. r", "Rep. N","Diff. (z)","SE diff (Fisher's z)")
    df$`Diff. (z)` = atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep)
    df$`Orig. N` = as.integer(df$`Orig. N`)
    df$`Rep. N` = as.integer(df$`Rep. N`)
  
    if(which.pt){
      return(df[which.pt,])
    }else{
      return(df)
    }
  })
   
  output$sumPlot <- renderPlot({
    
    
    if(input$prop_to_n1)
      cx = sqrt(pi_dat_nona$n_orig / min(pi_dat_nona$n_orig)*.5)
    else{
      cx = NULL
    }
    
    do.call(par,par.list)
    par(mfrow=c(1,1),mar=c(4,4,3.7,4))
    
    which.pt = as.integer(input$study)
    
    alpha = as.numeric(input$alpha)
    
    nsig = abs(atanh(pi_dat_nona$cor_orig)-atanh(pi_dat_nona$cor_rep))<(qnorm(1-alpha/2)*pi_dat_nona$se_total)
    
    
    if(which.pt==0){
    plot(atanh(pi_dat_nona$cor_orig),atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep),
         ylim=c(-1.4,1.4),ylab=c("Difference (Fisher's z)"),xlab="Original study's correlation",
         pch=21,col=NA,bg=rgb(!nsig,0,0,.3),xlim=c(-.3,atanh(.9)),
         axes=FALSE, cex = cx)
    se = qnorm(1-alpha/2)*pi_dat_nona$se_total
    segments(atanh(pi_dat_nona$cor_orig),
             atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep) + se,
             atanh(pi_dat_nona$cor_orig),
             atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep) - se,
             rgb(!nsig,0,0,.4))
      for(i in 1:nrow(spec_tab)){
        text(atanh(spec_tab$`Orig. r`[i]), spec_tab$`Deviation (Fisher's z)`[i],letters[i],adj=c(1,1),cex=1.3)
      }
    }else{
      plot(atanh(pi_dat_nona$cor_orig),atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep),
           ylim=c(-1.4,1.4),ylab=c("Difference (Fisher's z)"),xlab="Original study's correlation",
           pch=21,col=NA,bg=rgb(!nsig,0,0,.3),ty='n',xlim=c(-.3,atanh(.9)),
           axes=FALSE)
      points(atanh(pi_dat_nona$cor_orig[which.pt]),atanh(pi_dat_nona$cor_orig[which.pt]) - atanh(pi_dat_nona$cor_rep[which.pt]),
             pch=21,col=NA,bg=rgb(!nsig[which.pt],0,0,.3),ty='p',cex=cx[which.pt])
      se = qnorm(1-alpha/2)*pi_dat_nona$se_total
      segments(atanh(pi_dat_nona$cor_orig[which.pt]),
               atanh(pi_dat_nona$cor_orig[which.pt]) - atanh(pi_dat_nona$cor_rep[which.pt]) + se[which.pt],
               atanh(pi_dat_nona$cor_orig[which.pt]),
               atanh(pi_dat_nona$cor_orig[which.pt]) - atanh(pi_dat_nona$cor_rep[which.pt]) - se[which.pt],
               rgb(!nsig[which.pt],0,0,.4))
      
      sig = pi_dat_nona$pval[which.pt]<as.numeric(input$alpha)
      ci = atanh(pi_dat_nona$cor_orig[which.pt]) + c(-1,1)*qnorm(1-as.numeric(input$alpha)/2)/sqrt(pi_dat_nona$n_orig[which.pt] - 3)
      segments(ci[1],
               atanh(pi_dat_nona$cor_orig[which.pt]) - atanh(pi_dat_nona$cor_rep[which.pt]),
               ci[2],
               atanh(pi_dat_nona$cor_orig[which.pt]) - atanh(pi_dat_nona$cor_rep[which.pt]),
               rgb(as.numeric(sig),0,0,.4))
      
    }
    axis(2)
    axis(1, at = atanh(seq(0,.8,.2)),lab=seq(0,.8,.2))
    axis(3, at = seq(0,1.25,.25))
    mtext("Original study's Fisher's z",3,2.5,adj=.5)
    box()
    
    abline(0,1,lty=3,col="lightblue")
    at0 = -.25
    ang = 90 - tan(par()$pin[2]/par()$pin[1])*180/pi
    text(at0,atanh(at0)+.1,expression(r[rep]==0),srt=ang,cex=1.3,col="lightblue",adj=.5)
    
    abline(h=0,col=rgb(0,0,0,.3),lty=2,lwd=.8)
    abline(v=0,col=rgb(0,0,0,.3),lty=2,lwd=.8)
    axis(4,at=seq(-1,1,.5),lab=round(tanh(seq(-1,1,.5)),2),cex.axis=1)
    mtext("Difference (correlation, original r=0)",4,2.9,adj=.5,las=3)
    mtext("Orig. > Repl.",3,-1.5,cex=1.3,adj=.05)
    mtext("Repl. > Orig.",1,-1.5,cex=1.3,adj=.05)
    
    
  })
  
  output$powerPlot <- renderPlot({
    
    par(mfrow=c(2,1))
    do.call(par,par.list)
    
    
    alpha = as.numeric(input$alpha)
    refpow = as.numeric(input$refpow)
    xlims = as.numeric(input$xlim)
    true.d = input$diff
    zz = seq(xlims[1]-.25,xlims[2]+.25,len=400)
    
    
    if(!as.integer(input$study)){
      plot(0,0,axes=FALSE,ylab="",xlab="",ty='n')
      text(mean(par()$usr[1:2]),mean(par()$usr[3:4]),"Select a study.",cex=2)

    }else{
  

      ex.row = as.integer(input$study)
    
      n.o = pi_dat_nona[ex.row,"n_orig"]
      n.r = pi_dat_nona[ex.row,"n_rep"]
      r.o = pi_dat_nona[ex.row,"cor_orig"]
      r.r = pi_dat_nona[ex.row,"cor_rep"]
    
      z.d = atanh(r.o) - atanh(r.r)
      sd.d = pi_dat_nona[ex.row,"se_total"]

      

      y.expand = 1.1
    
      yy = dnorm(zz,true.d,sd.d)
      plot(zz, yy, ty='l', ylab="Density", xlab="Difference (Fisher's z)",axes=FALSE,ylim=c(0,y.expand*max(yy)),lwd=2
         ,xlim=xlims)
      box()
      axis(1)
      axis(3, at = seq(-2,2,.5), lab=round(tanh(seq(-2,2,.5)),2))
      mtext("Deviation (correlation, original r=0)",3,2.5,adj=.5)

      crit = qnorm(1-alpha/2, 0, sd.d)
      abline(v=c(-crit,crit),col="gray")
    
      ## Left side
      xx = seq(min(zz),-crit,len=400)
      yy2 = dnorm(xx,true.d,sd.d)
      polygon(c(xx,rev(xx)),c(yy2,yy2*0),col=rgb(1,0,0,.3))
      text(-crit,par()$usr[4],substitute(alpha==alp,list(alp=round(alpha,2))),
         srt=90,adj=c(1.1,1.2),col="gray")
      text(-crit,par()$usr[4],substitute(d[z]==r0,list(r0=round(-crit,2))),
         srt=90,adj=c(1.1,-.2),col="gray")
    
      ## right side
      xx = seq(crit, max(zz),len=400)
      yy2 = dnorm(xx,true.d,sd.d)
      polygon(c(xx,rev(xx)),c(yy2,yy2*0),col=rgb(1,0,0,.3))
      text(crit,par()$usr[4],substitute(alpha==alp,list(alp=round(alpha,2))),
         srt=90,adj=c(1.1,1.2),col="gray")
      text(crit,par()$usr[4],substitute(d[z]==r0,list(r0=round(crit,2))),
         srt=90,adj=c(1.1,-.2),col="gray")
    
      
      if(input$show_null){
        yy2 = dnorm(zz,0,sd.d)
        lines(zz, yy2, lty=2,lwd=2)
      }
    }  
    
    
    
    pows = sapply(1:nrow(pi_dat_nona),function(i){
      pow = (1-pnorm(-qnorm(alpha/2),zz/pi_dat_nona$se_total[i])) + 
        pnorm(qnorm(alpha/2),zz/pi_dat_nona$se_total[i])
    })
    pows0 = sapply(1:nrow(pi_dat_nona),function(i){
      pow = (1-pnorm(-qnorm(alpha/2),true.d/pi_dat_nona$se_total[i])) + 
        pnorm(qnorm(alpha/2),true.d/pi_dat_nona$se_total[i])
    })
    
    matplot(zz,pows,ylim=c(0,1),las=1,ylab=
              substitute(paste("Power (",alpha == alp, ")", sep="" ),list(alp = round(alpha,2))),
            xlab="Difference (Fisher's z)",col=rgb(0,0,0,.2),lwd=1,lty=1,xlim = xlims,ty='n')
    if(!as.integer(input$study)){
      good.pow = pows0>refpow
      matlines(zz,pows,col=rgb(0,0,good.pow,.2),lwd=1,lty=1)
      abline(v=c(-1,1)*as.numeric(input$diff),col="red")
      text(par()$usr[2],.25,paste(round(mean(good.pow)*100),"% have >",refpow," power",sep=""),adj=1.2)
      text(par()$usr[2],.15,paste("at a true diff. of ",true.d,".",sep=""),adj=1.2)
    }else{
      pow0 = (1-pnorm(-qnorm(alpha/2),input$diff/pi_dat_nona$se_total[as.integer(input$study)])) + 
        pnorm(qnorm(alpha/2),input$diff/pi_dat_nona$se_total[as.integer(input$study)])
      
      lines(zz,pows[,as.integer(input$study)],col=rgb(0,0,0,.5),lwd=1,lty=1)
      segments(input$diff,0,input$diff,pow0,col="red")
      segments(input$diff,pow0,par()$usr[1],pow0, col="red")
      points(input$diff,pow0,pch=19,col="red")
    }
    abline(h=refpow,col="gray",lty=2)
    abline(h=.5,col="gray",lty=2)
    
    axis(3, at = seq(-2,2,.5), lab=round(tanh(seq(-2,2,.5)),2))
    mtext("Deviation (correlation, original r=0)",3,2.5,adj=.5)
    #lines(zz,apply(pows,1,median),lwd=3,col="red",lty=1)
    #lines(zz,apply(pows,1,quantile,p=.25),lwd=3,col="blue",lty=1)
    #lines(zz,apply(pows,1,quantile,p=.75),lwd=3,col="blue",lty=1)
    
    if(input$show_null){
      abline(v=0,col="black",lty=2)
      points(0,alpha,pch=21,col="black",bg="white")
    }
    
    
    
    
  })
  
  output$pPlot <- renderPlot({
    par(mfrow=c(2,1))
    do.call(par,par.list)
    
    
    alpha = as.numeric(input$alpha)
    xlims = as.numeric(input$xlim)
    zz = seq(xlims[1]-.25,xlims[2]+.25,len=400)
    
    
    if(!as.integer(input$study)){
      plot(0,0,axes=FALSE,ylab="",xlab="",ty='n')
      text(mean(par()$usr[1:2]),mean(par()$usr[3:4]),"Select a study.",cex=2)
      
    }else{
      
      
      ex.row = as.integer(input$study)
      
      n.o = pi_dat_nona[ex.row,"n_orig"]
      n.r = pi_dat_nona[ex.row,"n_rep"]
      r.o = pi_dat_nona[ex.row,"cor_orig"]
      r.r = pi_dat_nona[ex.row,"cor_rep"]
      
      z.d = atanh(r.o) - atanh(r.r)
      sd.d = pi_dat_nona[ex.row,"se_total"]
      
      true.d = input$diff
      
      y.expand = 1.1
      
      yy = dnorm(zz,true.d,sd.d)
      plot(zz, yy, ty='l', ylab="Density", xlab="Difference (Fisher's z)",axes=FALSE,ylim=c(0,y.expand*max(yy)),lwd=2
           ,xlim=xlims)
      box()
      axis(1)
      axis(3, at = seq(-2,2,.5), lab=round(tanh(seq(-2,2,.5)),2))
      mtext("Deviation (correlation, original r=0)",3,2.5,adj=.5)
      
      #crit = qnorm(1-alpha/2, 0, sd.d)
      crit = abs(z.d)
      abline(v=c(-crit,crit),col="gray")
      
      ## Left side
      xx = seq(-crit,crit,len=400)
      yy2 = dnorm(xx,true.d,sd.d)
      polygon(c(xx,rev(xx)),c(yy2,yy2*0),col=rgb(1,0,0,.3))

      text(-crit,par()$usr[4],substitute(d[z]==r0,list(r0=round(-crit,2))),
           srt=90,adj=c(1.1,-.2),col="gray")
      
      text(crit,par()$usr[4],substitute(d[z]==r0,list(r0=round(crit,2))),
           srt=90,adj=c(1.1,-.2),col="gray")
      
      
      if(input$show_null){
        yy2 = dnorm(zz,0,sd.d)
        lines(zz, yy2, lty=2,lwd=2)
      }
    }  
    
    
    
    
    pows = 1-sapply(1:nrow(pi_dat_nona),function(i){
      crit =  abs(atanh(pi_dat_nona$cor_orig[i]) - atanh(pi_dat_nona$cor_rep[i]))
      pow = (1-pnorm(crit,zz,pi_dat_nona$se_total[i])) + 
        pnorm(-crit,zz,pi_dat_nona$se_total[i])
    })
    pows0 = 1-sapply(1:nrow(pi_dat_nona),function(i){
      crit =  abs(atanh(pi_dat_nona$cor_orig[i]) - atanh(pi_dat_nona$cor_rep[i]))
      pow = (1-pnorm(crit,input$diff,pi_dat_nona$se_total[i])) + 
        pnorm(-crit,input$diff,pi_dat_nona$se_total[i])
    })
    matplot(zz,pows,ylim=c(0,1),las=1,ylab=
              substitute(paste("p value (|d|>z)"),list(alp = round(alpha,2))),
            xlab="Difference (Fisher's z)",col=rgb(0,0,0,.2),lwd=1,lty=1,xlim = xlims,ty='n')
    if(!as.integer(input$study)){
      sig = pows0<alpha
      matlines(zz,pows,col=rgb(0,0,sig,.4),lwd=1,lty=1)
      abline(v=c(-1,1)*as.numeric(input$diff),col="red")
      text(par()$usr[2],.85,paste(round(mean(sig)*100),"% have p<",round(alpha,2),sep=""),adj=1.2)
      text(par()$usr[2],.75,paste("for the test that |d|>",input$diff,".",sep=""),adj=1.1)
      
    }else{
      pow0 = (1-pnorm(crit,input$diff,pi_dat_nona$se_total[as.integer(input$study)])) + 
        pnorm(-crit,input$diff,pi_dat_nona$se_total[as.integer(input$study)])
      pow0 = 1-pow0
      lines(zz,pows[,as.integer(input$study)],col=rgb(0,0,0,.5),lwd=1,lty=1)
      segments(input$diff,0,input$diff,pow0,col="red")
      segments(input$diff,pow0,par()$usr[1],pow0, col="red")
      points(input$diff,pow0,pch=19,col="red")
    }
    
    abline(h=c(alpha,1-alpha),col="gray",lty=2)
    axis(3, at = seq(-2,2,.5), lab=round(tanh(seq(-2,2,.5)),2))
    mtext("Deviation (correlation, original r=0)",3,2.5,adj=.5)
    #lines(zz,apply(pows,1,median),lwd=3,col="red",lty=1)
    #lines(zz,apply(pows,1,quantile,p=.25),lwd=3,col="blue",lty=1)
    #lines(zz,apply(pows,1,quantile,p=.75),lwd=3,col="blue",lty=1)
    
    if(input$show_null){
      abline(v=0,col="black",lty=2)
      if(as.integer(input$study)){
        pow0 = (1-pnorm(crit,0,pi_dat_nona$se_total[as.integer(input$study)])) + 
          pnorm(-crit,0,pi_dat_nona$se_total[as.integer(input$study)])
        pow0 = 1-pow0
        points(0,pow0,pch=21,col="black",bg="white")
      }
    }
    
    
    
    
    
    
  })
  

})  
  