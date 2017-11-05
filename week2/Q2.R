
library(actuar)
library(VGAM)

setwd("C:/Users/lenovo/Desktop/652/Electronic homework/work2")
MEPS1 <- read.csv("HealthExpend.csv")
MEPS <- subset(MEPS1,EXPENDOP>0)

#####fit.gamma
fit.gamma <- vglm(EXPENDOP ~ 1,family=gamma2,loc=0,data=MEPS)
coef(fit.gamma)
(theta1<-exp(coef(fit.gamma)[1])/exp(coef(fit.gamma)[2])) #mu=theta/alpha
(alpha1<-exp(coef(fit.gamma)[2]))

plot(density(log(MEPS$EXPENDOP)), main="", xlab="Log EXPENDOP",ylim=c(0,0.3),col=1)
x <- seq(0,15,by=0.01)
fgamma_ex = dgamma(exp(x), shape = alpha1, scale=theta1)*exp(x)
lines(x,fgamma_ex,col=2)

scaleparam1 <- c(theta1,theta1-1000,theta1+1000)
for(k in 2:length(scaleparam1))
{
  fgamma_ex = dgamma(exp(x), shape = alpha1, scale=scaleparam1[k])*exp(x)
  lines(x,fgamma_ex,col=k+1)
}
legend("topright", c("histogram",paste("scale=",as.character(scaleparam1[1])), 
                     paste("scale=",as.character(scaleparam1[2])),
                     paste("scale=",as.character(scaleparam1[3]))), 
                     lty=1,bty="n",col=1:4,cex=0.7)
title("Pdf gamma density,with varying scale")


#######fit.paretoII
fit.pareto <- vglm(EXPENDOP ~ 1,family=paretoII,loc=0,data=MEPS)
coef(fit.pareto)
theta2 <- exp(coef(fit.pareto))[1]
alpha2 <- exp(coef(fit.pareto))[2]

plot(density(log(MEPS$EXPENDOP)), main="", xlab="Log EXPENDOP",xlim=c(0,14),ylim=c(0,0.3),col=1)
x <- seq(0,20,by=0.01)
fpareto_ex = dparetoII(exp(x), shape = alpha2, scale=theta2)*exp(x)
lines(x,fpareto_ex,col=2)

scaleparam2 <- c(theta2,theta2-400,theta2+400)
for(k in 2:length(scaleparam2))
{
  fpareto_ex = dparetoII(exp(x), shape = alpha2, scale=scaleparam2[k])*exp(x)
  lines(x,fpareto_ex,col=k+1)
}
legend("topright", c("histogram",paste("scale=",as.character(scaleparam2[1])), 
                    paste("scale=",as.character(scaleparam2[2])),
                    paste("scale=",as.character(scaleparam2[3]))), 
                   lty=1,bty="n",col=1:4,cex=0.7)
title("Pdf pareto density,with varying scale")


