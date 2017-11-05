setwd("C:/Users/lenovo/Desktop/652/Electronic homework/work3")
MEPS1 <- read.csv("HealthExpend.csv",header=TRUE)
MEPS <- subset(MEPS1,EXPENDOP>0)

library(MASS)
library(VGAM)
#  Inference assuming a gamma Distribution
fit.gamma2 <- glm(EXPENDOP~1, data=MEPS,family=Gamma(link=log))
(theta<-exp(coef(fit.gamma2))*gamma.dispersion(fit.gamma2)) #mu=theta/alpha
(alpha<-1/gamma.dispersion(fit.gamma2) )
summary(fit.gamma2, dispersion = gamma.dispersion(fit.gamma2)) 

#  Inference assuming a Pareto Distribution
fit.pareto <- vglm(EXPENDOP~ 1, paretoII, loc=0, data = MEPS)
exp(coef(fit.pareto))
summary(fit.pareto)
head(fitted(fit.pareto))


# Plotting the fit using densities (on a logarithmic scale)
x <- seq(0,12,by=0.01)
plot(density(log(MEPS$EXPENDOP)) ,main="", xlab="Log Expenditures",ylim=c(0, 0.3))
fgamma_ex = dgamma(exp(x), shape = alpha, scale=theta)*exp(x)
lines(x,fgamma_ex,col="blue")
fpareto_ex = dparetoII(exp(x),loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))*exp(x)
lines(x,fpareto_ex,col="purple")
legend("topright", c("gamma", "pareto"), lty=1, col = c("blue","purple"))

#  PP Plot
Percentiles  <- ecdf(MEPS$EXPENDOP)
par(mfrow=c(1, 2))
Fgamma_ex = pgamma(MEPS$EXPENDOP, shape = alpha, scale=theta)
plot(Percentiles(MEPS$EXPENDOP),Fgamma_ex, xlab="Empirical DF", ylab="Gamma DF",cex=0.4)
abline(0,1)
Fpareto_ex = pparetoII(MEPS$EXPENDOP,loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))
plot(Percentiles(MEPS$EXPENDOP),Fpareto_ex, xlab="Empirical DF", ylab="Pareto DF",cex=0.4)
abline(0,1)

#Kolmogorov-Smirnov # the test statistic is "D"
library(goftest)
ks.test(MEPS$EXPENDOP, "pgamma", shape = alpha, scale=theta)
ks.test(MEPS$EXPENDOP, "pparetoII",loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))





