#Stat 340 Assign 5 Q3
#Part 1


MNTL = function(n)
{
  #n is the number of iterations.  If n=1 then MNTL lost power one time.
  #  IF n=2 then MNTL loses power twice.  
  
  L = NULL  #An empty output vector containing the lifetimes from 1 to n.
  
  for (i in 1:n)
  {
    T = rexp(8,1/20)  #Lives of our 8 trans. lines.
    
    P1 = min(T[1],T[2],T[7])
    P2 = min(T[1:4],T[8])
    P3 = min(T[1],T[5:6],T[8])
    P4 = min(T[1],T[3:7])

    
    life = max(P1,P2,P3,P4)
    
    L = c(L,life)
    
  }  #end the for loop
  
  return(L)  #return the lifetimes!
  
}  #end the function


chat_dis = function(n)
{
  #n is the number of iterations.  If n=1 then MNTL lost power one time.
  #  IF n=2 then MNTL loses power twice.  
  
  L = NULL  #An empty output vector containing the lifetimes from 1 to n.
  
  for (i in 1:n)
  {
    T = rexp(8,1/20)  #Lives of our 8 trans. lines.
    
    P3 = min(T[1],T[5:6],T[8])
    P4 = min(T[1],T[3:7])
    
    
    life = max(P3,P4)
    
    L = c(L,life)
    
  }  #end the for loop
  
  return(L)  #return the lifetimes!
  
}  #end the function

#finding the estimate for the proportion
a=MNTL(100)
b=chat_dis(100)
proportion=b/a
est=mean(proportion)
c=1.96
se=sqrt(est*(100-est)/100)
#Finding the 95% CI for the proportion
CI=c(est-c*se,est+c*se)
CI



# Part 2

MNTL_new = function(n)
{
  #n is the number of iterations.  If n=1 then MNTL lost power one time.
  #  IF n=2 then MNTL loses power twice.  
  
  L = NULL  #An empty output vector containing the lifetimes from 1 to n.
  
  for (i in 1:n)
  {
     T147 = rnorm(3,8,1) #Lives of lines 1,4 and 7
     u=runif(4)
     T2356 = 1/u
     T8 = rexp(1,1/20)  #Lives of line 8
    
    P1 = min(T147[1],T2356[1],T147[3]) #path 1,2,7
    P2 = min(T147[1:2],T2356[1:2],T8[1]) #path 1-4,8
    P3 = min(T147[1],T2356[3:4],T8[1]) #path 1,5,6,8
    P4 = min(T147[1],T147[2:3],T2356[2:4]) #path 1,3-7
    
    
    life = max(P1,P2,P3,P4)
    
    L = c(L,life)
    
  }  #end the for loop
  
  return(L)  #return the lifetimes!
  
}  #end the function


chat_dis_new = function(n)
{
  #n is the number of iterations.  If n=1 then MNTL lost power one time.
  #  IF n=2 then MNTL loses power twice.  
  
  L = NULL  #An empty output vector containing the lifetimes from 1 to n.
  
  for (i in 1:n)
  {
    T147 = rnorm(3,8,1) #Lives of lines 1,4 and 7
    u=runif(4)
    T2356 = 1/u
    T8 = rexp(1,1/20)  #Lives of line 8
    
    P3 = min(T147[1],T2356[3:4],T8[1]) #path 1,5,6,8
    P4 = min(T147[1],T147[2:3],T2356[2:4]) #path 1,3-7
    
    
    life = max(P3,P4)
    
    L = c(L,life)
    
  }  #end the for loop
  
  return(L)  #return the lifetimes!
  
}  #end the function

#Estimate of new proportion
c=MNTL_new(100)
d=chat_dis_new(100)
proportion_new=d/c
est_new=mean(proportion_new)
est_new


#Is the reality different than the given story?
#hypotheses test

#H0: est=est_new (est-est_new = 0)
#HA: est does not equal est_new

#discrepancy
pooled_est=(100*est+100*est_new)/200
d=(est-est_new)/sqrt((100-pooled_est)*pooled_est*(1/100+1/100))
d

#p-value
pvalue=2*(1-pnorm(abs(d)))
pvalue

#since pvalue > 0.1 we do not reject H0
#Hence the reality is same as the given story
