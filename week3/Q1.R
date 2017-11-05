library(moments)
set.seed(2) #set seed to reproduce work 
nTot<-20000  #number of simulations
alpha<-2
theta<-150

#simulation
Losses<-rgamma(nTot,alpha,scale = theta)  

###Raw moments for k=1
# Theoretical 
k<-1
((theta^k)*gamma(alpha+k))/gamma(alpha)

#Simulated data raw moments
moment(Losses, order = k, central = FALSE)


###Raw moments for k=2
# Theoretical 
k<-2
((theta^k)*gamma(alpha+k))/gamma(alpha)

#Simulated data raw moments
moment(Losses, order = k, central = FALSE)