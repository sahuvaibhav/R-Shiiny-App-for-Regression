panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}


normal = function(mean, sd, lb,ub){
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  plot(x, hx, type="n", ylab="",
       main="Normal Distribution", axes=FALSE)
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red",border = 1)
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb,"< Mu <",ub,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(mean-4*sd, mean+4*sd, sd), pos=0) 
}


hypothesis_test = function(mean,SmeanHT,sd,sdchoice,type,alpha,n){
  x <- seq(-4,4,length=100)*sd/sqrt(n) + mean
  hx <- dnorm(x,mean,sd/sqrt(n))
  
  if (type == '1') {
    plot(x, hx, type="n", ylab="",xlab = "Xbar",main="Two Tail Hypothesis Test", axes=FALSE)
    if (sdchoice == '1'){
      zl = qnorm(alpha/2,mean,sd/sqrt(n))
      zu = qnorm(1-alpha/2,mean,sd/sqrt(n))
    } else {
      zl = mean+ qt(alpha/2,n-1)*sd/sqrt(n)
      zu = mean+qt(1-alpha/2,n-1)*sd/sqrt(n)
    }
    i <- x <= zl 
    j  = x >=zu
    lines(x, hx)
    polygon(c(min(x),x[i],zl), c(0,hx[i],0), col="red")
    polygon(c(zu,x[j],max(x)), c(0,hx[j],0), col="red")
    axis(1, at=c(zl,zu,mean-3*sd/sqrt(n), mean-2*sd/sqrt(n),mean-sd/sqrt(n),mean,
                 mean+sd/sqrt(n),mean+2*sd/sqrt(n),mean+3*sd/sqrt(n),SmeanHT), pos=0) 
    
    text(x=zl, y=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zl,mean,sd/sqrt(n)))/1.2, labels='Rejection Region', col='blue')
    arrows(x0=zl, y0=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zl,mean,sd/sqrt(n)))/1.2, x1=zl-0.3*sd/sqrt(n), y1=dnorm(zl-0.3*sd/sqrt(n),mean, sd/sqrt(n)), col='blue', length=0.1, lwd=1)
    
    text(x=zu, y=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zu,mean,sd/sqrt(n)))/1.2, labels='Rejection Region', col='blue')
    arrows(x0=zu, y0=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zu,mean,sd/sqrt(n)))/1.2, x1=zu+0.3*sd/sqrt(n), y1=dnorm(zu+0.3*sd/sqrt(n),mean, sd/sqrt(n)), col='blue', length=0.1, lwd=1)
    
    text(x=SmeanHT, y=(dnorm(mean,mean,sd/sqrt(n)) - 0)/1.6, labels='Sample Mean', col='Green')
    arrows(x0=SmeanHT, y0=(dnorm(mean,mean,sd/sqrt(n)) - 0)/1.6, x1=SmeanHT, y1=0, col='Green', length=0.1, lwd=1)
  }
  if (type == '2') {
    plot(x, hx, type="n", ylab="",xlab = "Xbar",
         main="Left Tail Hypothesis Test", axes=FALSE)
    if (sdchoice == '1'){
      zl = qnorm(alpha,mean,sd/sqrt(n))
    } else {
      zl = mean+ qt(alpha,n-1)*sd/sqrt(n)
    }
    i <- x <= zl 
    lines(x, hx)
    polygon(c(min(x),x[i],zl), c(0,hx[i],0), col="red")
    axis(1, at=c(mean-3*sd/sqrt(n), mean-2*sd/sqrt(n),mean-sd/sqrt(n),mean,
                 mean+sd/sqrt(n),mean+2*sd/sqrt(n),mean+3*sd/sqrt(n),zl,SmeanHT), pos=0)
    text(x=zl, y=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zl,mean,sd/sqrt(n)))/1.2, labels='Rejection Region', col='blue')
    arrows(x0=zl, y0=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zl,mean,sd/sqrt(n)))/1.2, x1=zl-0.3*sd/sqrt(n), y1=dnorm(zl-0.3*sd/sqrt(n),mean, sd/sqrt(n)), col='blue', length=0.1, lwd=1)
    text(x=SmeanHT, y=(dnorm(mean,mean,sd/sqrt(n)) - 0)/1.6, labels='Sample Mean', col='Green')
    arrows(x0=SmeanHT, y0=(dnorm(mean,mean,sd/sqrt(n)) - 0)/1.6, x1=SmeanHT, y1=0, col='Green', length=0.1, lwd=1)
  }
  
  if (type == '3') {
    plot(x, hx, type="n", ylab="",xlab = "Xbar",
         main="Right Tail Hypothesis Test", axes=FALSE)
    if (sdchoice == '1'){
      zu = qnorm(1-alpha,mean,sd/sqrt(n))
    } else {
      zu = mean+qt(1-alpha,n-1)*sd/sqrt(n)
    }
    j  = x >=zu
    lines(x, hx)
    polygon(c(zu,x[j],max(x)), c(0,hx[j],0), col="red")
    axis(1, at=c(mean-3*sd/sqrt(n), mean-2*sd/sqrt(n),mean-sd/sqrt(n),mean,
                 mean+sd/sqrt(n),mean+2*sd/sqrt(n),mean+3*sd/sqrt(n),zu,SmeanHT), pos=0)
    text(x=zu, y=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zu,mean,sd/sqrt(n)))/1.2, labels='Rejection Region', col='blue')
    arrows(x0=zu, y0=(dnorm(mean,mean,sd/sqrt(n)) - dnorm(zu,mean,sd/sqrt(n)))/1.2, x1=zu+0.3*sd/sqrt(n), y1=dnorm(zu+0.3*sd/sqrt(n),mean, sd/sqrt(n)), col='blue', length=0.1, lwd=1)
    text(x=SmeanHT, y=(dnorm(mean,mean,sd/sqrt(n)) - 0)/1.6, labels='Sample Mean', col='Green')
    arrows(x0=SmeanHT, y0=(dnorm(mean,mean,sd/sqrt(n)) - 0)/1.6, x1=SmeanHT, y1=0, col='Green', length=0.1, lwd=1)
  }
}

hyp_test_res = function(mean,SmeanHT,sd,sdchoice,type,alpha,n){
  if (type == '1'){
    if (sdchoice == '1'){
      zl = round(qnorm(alpha/2,mean, sd/sqrt(n)),5)
      zu = round(qnorm(1-alpha/2,mean, sd/sqrt(n)),5)
      p_value = round(pnorm(SmeanHT,mean,sd/sqrt(n)),5)
      if (p_value >0.5){
        p_value = 2*(1- p_value)
      } else {
        p_value = 2*p_value }
    } else {
      zl = round(mean + qt(alpha/2,n-1)*sd/sqrt(n),5)
      zu = round(mean + qt(1-alpha/2,n-1)*sd/sqrt(n),5)
      p_value = round(pt((SmeanHT-mean)*sqrt(n)/sd, n-1),5)
      if (p_value >0.5){
        p_value = 2*(1- p_value)
      } else {
        p_value = 2*p_value }
    }
    if (p_value <= alpha)    {
      text = paste("As p-value is less than alpha(for 2 tail test) we can reject null hypothesis")        
    }
    if (p_value > alpha)    {
      text = paste("As p-value is greater than alpha(for 2 tail test) we can NOT reject null hypothesis")        
    }
    text0 = paste("p-value:" , p_value)
    text2 = paste("Critical Values:" , zl ,"," ,zu)
    
  }
  
  if (type == '2'){
    if (sdchoice == "1"){
      zl = round(qnorm(alpha,mean, sd/sqrt(n)),5)
      p_value = round(pnorm(SmeanHT,mean,sd/sqrt(n)),5)
    } else {
      zl = round(mean + qt(alpha,n-1)*sd/sqrt(n),5)
      p_value = round(pt((SmeanHT-mean)*sqrt(n)/sd, n-1),5)
    } 
    
    if (p_value <= alpha)    {
      text = paste("As p-value is less than alpha we can reject the null hypothesis(H0).")        
    }
    if (p_value > alpha)    {
      text = paste("As p-value is greater than alpha we can NOT reject the null hypothesis(H0)")        
    }
    text0 = paste("p-value:" , p_value)
    text2 = paste("Critical Value:" , zl)
    
  }
  if (type == '3'){
    if(sdchoice == '1') {
      zu = round(qnorm(1-alpha,mean, sd/sqrt(n)),5)
      p_value =round(1- pnorm(SmeanHT,mean,sd/sqrt(n)),5)
    } else {
      zu = round(mean + qt(1-alpha,n-1)*sd/sqrt(n),5)
      p_value =round(1- pt((SmeanHT-mean)*sqrt(n)/sd, n-1),5)
      
    }
    if (p_value <= alpha)    {
      text = paste("As p-value is less than alpha we can reject the null hypothesis(H0).")        
    }
    if (p_value > alpha)    {
      text = paste("As p-value is greater than alpha we can NOT reject the null hypothesis(H0)")        
    }
    text0 = paste("p-value:" , p_value)
    text2 = paste("Critical Value:" ,zu)
  }
  
  return(c(text0,text,text2))
  
}