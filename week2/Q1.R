
library(actuar)
library(VGAM)

dSpliceGamPar <- function(x,c,v,alpha1,theta1,alpha2,theta2)
{
  if(0<=x & x<c) {return(v*dgamma(x,shape=alpha1,scale=theta1)/pgamma(c,shape=alpha1,scale=theta1))} 
  if(x>=c) {return((1-v)*dparetoII(x,loc=0,shape=alpha2,scale=theta2)/(1-pparetoII(c,loc=0,shape=alpha2,scale=theta2)))}
}

x <- t(as.matrix(1:9500/10))

dSpliceValues <- apply(x,2,dSpliceGamPar,c=300,v=0.3,alpha1=2,theta1=75,alpha2=2,theta2=400)

plot(x,dSpliceValues,type="l")
title("Spliced distribution")

pSpliceGamPar <- function(x,c,v,alpha1,theta1,alpha2,theta2)
{
  if(0<=x & x<c) {return(v*pgamma(x,shape=alpha1,scale=theta1)/pgamma(c,shape=alpha1,scale=theta1))}
  if(x>=c) {return(v+(1-v)*(pparetoII(x,loc=0,shape=alpha2,scale=theta2)-pparetoII(c,loc=0,shape=alpha2,scale=theta2))/(1-pparetoII(c,loc=0,shape=alpha2,scale=theta2)))}
}
probability <- 1-pSpliceGamPar(200,c=300,v=0.3,alpha1=2,theta1=75,alpha2=2,theta2=400)
