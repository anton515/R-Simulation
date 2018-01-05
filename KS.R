# Question 3, a)

RNORM<-function(a)
  #input is a realization of a uniform r.v.
{
  #Generate a Uniform(0,2*pi)
  u=a
  theta=2*pi*u
  
  #Generate a Exp(1/2)
  u=a
  Rsq=-2*log(u,base=exp(1))
  
  #Generating a r.v. that is N(0,1)
  theta=theta
  Rsq=Rsq
  x=sqrt(Rsq)*cos(theta)
  y=sqrt(Rsq)*sin(theta)
  return (c(x,y))
}

#Output for the two Uniform r.v. realizaitons 0.1, 0.5.
test1=RNORM(0.1) #output will be N(0,1) and ind. of each other
test2=RNORM(0.5) #output will be N(0,1) and ind. of each other
test1
test2

#Question 3, b)
#Performing a KS-Test for N(0,1)

KS_normal=function(data) 
{
  #Initializations
  n=length(data)
  N=1000
  F_upper=c(1:n)/n
  F_lower=F_upper-1/n
  #Parameter Estimation for Normal
  mu=mean(data)
  sigma=sd(data)
  #Calculating the ksd, d
  data=sort(data)
  F=pnorm(data,mu,sigma)
  d=max(abs(F-F_upper),abs(F-F_lower))
  ddots=NULL
  for (i in 1:N)
  {
    #simulating the KSD, ddots
    newdata = sort(rnorm(n,mu,sigma))
    newmu=mean(newdata)
    newsigma=sd(newdata)
    F=pnorm(newdata,newmu,newsigma)
    ddot=max(abs(F-F_upper),abs(F-F_lower))
    ddots=c(ddots,ddot)
  }
  pvalue=length(ddots[ddots>d])/N
  cat("\n","H0: The data is Normal with mean 0 and standard 
      deviation 1","\n")
  cat("\n","Ha: The data is NOT Normal with mean 0 and 
      standard deviation 1","\n")
  cat("\n","The pvalue is: ",pvalue,"\n")
  if (pvalue<0.10) print("reject H0")
  else print ("do not reject H0")
  cat("\n","We tested a mean of, ",mu,"and a standard
      deviation of,",sigma)
  cat("\n","We expected a mean of, sum of means, and a standard
      deviation of, root sum of variance","\n")
}

KS_normal(test1)
KS_normal(test2)


#Question 3, c) i)

RNORM2forchisq2<-function(n)
  #input is the number of runif r.v realizations
{
  #Generate a Uniform(0,2*pi)
  u=runif(n)
  v=runif(n)
  theta=2*pi*u
  theta2=2*pi*v
  
  #Generate a Exp(1/2)
  u=runif(n)
  v=runif(n)
  Rsq=-2*log(u,base=exp(1))
  Rsq2=-2*log(v,base=exp(1))
  
  #Generating a r.v. that is N(0,1)
  theta=theta
  Rsq=Rsq
  theta2=theta2
  Rsq2=Rsq2
  x1=sqrt(Rsq)*cos(theta)
  y1=sqrt(Rsq)*sin(theta)
  x2=sqrt(Rsq2)*cos(theta2)
  y2=sqrt(Rsq2)*sin(theta2)
  normrv=(c(x1,y1)) #gen N(0,1) r.v.s
  normrv2=(c(x2,y2)) #gen N(0,1) r.v.s
  return (normrv^2 + normrv2^2) #gen chisq(2) r.v.s
}

#Output chisq(2) r.v.s
g<-RNORM2forchisq2(100)


#Question 3, c) ii)

RNORM2forN49<-function(n)
  #input is the number of runif r.v realizations
{
  #Generate a Uniform(0,2*pi)
  u=runif(n)
  theta=2*pi*u
  
  #Generate a Exp(1/2)
  u=runif(n)
  Rsq=-2*log(u,base=exp(1))
  
  #Generating a r.v. that is N(0,1)
  theta=theta
  Rsq=Rsq
  x1=sqrt(Rsq)*cos(theta)
  y1=sqrt(Rsq)*sin(theta)
  normrv=(c(x1,y1)) #gen N(0,1) r.v.s
  return (3*normrv+4) #gen N(4,9) r.v.s
}

#output N(4,9) r.v.s
h<-RNORM2forN49(100)



#Question 3, c) iii)

RNORM2forGam<-function(n)
  #input is the number of runif r.v realizations
{
  #Generate a Uniform(0,2*pi)
  u=runif(n)
  theta=2*pi*u
  
  #Generate a Exp(1/2)
  u=runif(n)
  Rsq=-2*log(u,base=exp(1))
  
  #Generating a r.v. that is N(0,1)
  theta=theta
  Rsq=Rsq
  x1=sqrt(Rsq)*cos(theta)
  y1=sqrt(Rsq)*sin(theta)
  normrv=(c(x1,y1)) #gen N(0,1) r.v.s
  return (normrv^2) #gen Gam(1/2,1/2) r.v.s
}

#output Gam(1/2,1/2) r.v.s
j<-RNORM2forGam(100)


#Question 3, c) 4)

RNORM2forExp<-function(n)
  #input is the number of runif r.v realizations
{
  #Generate a Uniform(0,2*pi)
  u=runif(n)
  v=runif(n)
  theta=2*pi*u
  theta2=2*pi*v
  
  #Generate a Exp(1/2)
  u=runif(n)
  v=runif(n)
  Rsq=-2*log(u,base=exp(1))
  Rsq2=-2*log(v,base=exp(1))
  
  #Generating a r.v. that is N(0,1)
  theta=theta
  Rsq=Rsq
  theta2=theta2
  Rsq2=Rsq2
  x1=sqrt(Rsq)*cos(theta)
  y1=sqrt(Rsq)*sin(theta)
  x2=sqrt(Rsq2)*cos(theta2)
  y2=sqrt(Rsq2)*sin(theta2)
  normrv=(c(x1,y1)) #gen N(0,1) r.v.s
  normrv2=(c(x2,y2)) #gen N(0,1) r.v.s
  gamhalfhalf=normrv^2 #gen Gam(1/2,1/2) r.v.s
  gamhalfhalf2=normrv^2 #gen Gam(1/2,1/2) r.v.s
  Gam1half=gamhalfhalf + gamhalfhalf2 
  #Gen Gam(1,1/2) r.v, can add "alpha" since ind.
  #which is also Exp(1/2) distribution
  return(Gam1half/4)
  #Gen Exp(2) r.v.s
}

#output exp(2) r.v.s
k<-RNORM2forExp(100)



