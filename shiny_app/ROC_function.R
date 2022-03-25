ROC<-function(x,event,auc,cutoff,m=100,listdata=F,plot=T)
{
  ##  Author: Gillian Heller
  ##  17 January 2008
  ##
  ##  Computes and plots ROC curve
  ##  x = fitted values from logistic regression
  ##  event = binary response
  ##  m=no of points on curve
  ##
  ## Modified by K Bell
  ## 20 March 2002
  ## plot modifications--abline,title,legend
  ## added auc,cutoff parameters in function call
  start <- min(x)
  end <- max(x)
  
  sens<-onemspec<-1
  
  for (xi in seq(start+0.000001,end-0.000001,by=(end-start)/m))
  {
    tab<-table(event,x>xi)
    if( dim(tab)[2]==1) tab<-cbind(c(0,0),tab)
    sens<-c(sens,tab[2,2]/(tab[2,2]+tab[2,1]))
    onemspec<-c(onemspec,tab[1,2]/(tab[1,2]+tab[1,1]))
  }
  
  sens<-c(sens,0)
  onemspec<-c(onemspec,0)
  if(plot)
  {plot(onemspec,sens,type="n",xlab="1-specificity",ylab="Sensitivity",
        main = paste0("ROC Curve with ",cutoff," threshold"))
    lines(lowess(onemspec,sens,f=0.2))
    abline(0,1 , lty = 2)
    legend(x="topleft" , c(c("Model","Diagonal Line"),
                           c(paste0("AUC = ",auc),"AUC = 0.5")),
           lty = c(1, 2,0,0),horiz = FALSE, ncol = 2, cex = .75)
  }
  if(listdata)list(sens=sens,onemspec=onemspec)
}