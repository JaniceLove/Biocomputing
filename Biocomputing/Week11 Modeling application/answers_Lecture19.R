### Examples and answers for Lecture 19 challenges

#example 1
### Load the deSolve package and ggplot2 for plotting
library(deSolve)
library(ggplot2)

### Custom function that defines the model differential equations
# the ode function we will use for simulating the model requires the function be defined 
# with the arguments for time, state variable vector, and parameter vector be provided in that order
# ode also requires that the dy/dt's be returned as a list
ddSim<-function(t,y,p){
  # "unpack" vectors containing state variables (y) and parameters (p)
  N=y
  r=p[1]
  K=p[2]
  
  # calculate change in state variables with time, given parameter values 
  # and current value of state variables
  dNdt=r*(1-N/K)*N
  
  # return list containing change in state variables with time
  return(list(dNdt))
}

### Define parameters, initial values for state variables, and time steps
params=c(0.2,100)
N0=2
times=1:100

### Simulate the model using ode()
modelSim=ode(y=N0,times=times,func=ddSim,parms=params)

# modelSim is a variable that contains a deSolve object
# this contains some attributes about the way the simulation was conducted, but the key content is a
# matrix with time as the first column and model state variables in subsequent columns

# convert to a dataframe for plotting purposes
modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])

# plot output of simulation
ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic()


# example 2
predPreySim<-function(t,y,p){
  prey=y[1]
  pred=y[2]
  
  r=p[1]
  K=p[2]
  consume=p[3]
  predDeath=p[4]
  
  dPrey_dt=r*(1-prey/K)*prey-consume*prey*pred
  dPred_dt=consume*prey*pred-predDeath*pred
  
  return(list(c(dPrey_dt,dPred_dt)))
}

y0=c(25,1)
times=1:500
params=c(0.4,100,0.004,0.3)

out=ode(y=y0,times=times,func=predPreySim,parms=params)
out=data.frame(time=out[,1],prey=out[,2],pred=out[,3])
ggplot(out,aes(x=time,y=prey))+geom_line()+geom_line(data=out,mapping=aes(x=time,y=pred),col='red')+theme_classic()

# Challenge 1
tumorSim<-function(t,y,p){
  N=y[1]
  T=y[2]
  
  RN=p[1]
  KN=p[2]
  aTN=p[3]
  RT=p[4]
  KT=p[5]
  aNT=p[6]
  
  dNdt=RN*(1-(N+aNT*T)/KN)*N
  dTdt=RT*(1-(T+aTN*N)/KT)*T

  return(list(c(dNdt,dTdt)))
}

# case 2
times=1:100
y0=c(0.1,0.1)
params2=c(0.1,10,2,0.1,10,0.5)
sim2=ode(y=y0,times=times,func=tumorSim,parms=params2)
out2=data.frame(time=sim2[,1],normal=sim2[,2],tumor=sim2[,3])
ggplot(out2,aes(x=time,y=normal))+geom_line()+geom_line(data=out2,mapping=aes(x=time,y=tumor),col='red')+theme_classic()

# case 3
times=1:400
y0=c(0.05,0.3)
params3=c(0.1,10,0.5,0.1,10,0.5)
sim3=ode(y=y0,times=times,func=tumorSim,parms=params3)
out3=data.frame(time=sim3[,1],normal=sim3[,2],tumor=sim3[,3])
ggplot(out3,aes(x=time,y=normal))+geom_line()+geom_line(data=out3,mapping=aes(x=time,y=tumor),col='red')+theme_classic()

# case 4
times=1:100
y0=c(0.1,0.1)
params4=c(0.1,10,0.5,0.1,10,2)
sim4=ode(y=y0,times=times,func=tumorSim,parms=params4)
out4=data.frame(time=sim4[,1],normal=sim4[,2],tumor=sim4[,3])
ggplot(out4,aes(x=time,y=normal))+geom_line()+geom_line(data=out4,mapping=aes(x=time,y=tumor),col='red')+theme_classic()

# Challenge 2
# 2 state variables and 2 differential equations
# dN1dt=r1*(1-alpha11*N1-alpha12*N2)*N1
# dN2dt=r2*(1-alpha22*N2-alpha21*N1)*N2

lvSim<-function(t,y,p){
  N1=y[1]
  N2=y[2]

  r1=p[1]
  alpha11=p[2]
  alpha21=p[3]
  r2=p[4]
  alpha22=p[5]
  alpha12=p[6]
  
  dN1dt=r1*(1-alpha11*N1-alpha12*N2)*N1
  dN2dt=r2*(1-alpha22*N2-alpha21*N1)*N2
  
  return(list(c(dN1dt,dN2dt)))
}

# coexistence - alpha12<alpha11 & alpha21<alpha22
times=1:100
params=c(0.5,0.01,0.005,0.5,0.02,0.005)
y0=c(2,2)
sim=ode(y=y0,times=times,func=lvSim,parms=params)

sim=data.frame(t=sim[,1],N1=sim[,2],N2=sim[,3])
ggplot(sim,aes(x=t,y=N1))+geom_line()+geom_line(data=sim,mapping=aes(x=t,y=N2),col='red')+theme_classic()

# competitive exclusion by N1 - alpha21 > alpha11
times=1:100
params=c(0.5,0.01,0.015,0.5,0.02,0.005)
y0=c(2,2)
sim=ode(y=y0,times=times,func=lvSim,parms=params)

sim=data.frame(t=sim[,1],N1=sim[,2],N2=sim[,3])
ggplot(sim,aes(x=t,y=N1))+geom_line()+geom_line(data=sim,mapping=aes(x=t,y=N2),col='red')+theme_classic()

# competitive exclusion by N2 - alpha12 > alpha22
times=1:100
params=c(0.5,0.01,0.005,0.5,0.02,0.025)
y0=c(2,2)
sim=ode(y=y0,times=times,func=lvSim,parms=params)

sim=data.frame(t=sim[,1],N1=sim[,2],N2=sim[,3])
ggplot(sim,aes(x=t,y=N1))+geom_line()+geom_line(data=sim,mapping=aes(x=t,y=N2),col='red')+theme_classic()

# competitive exlusion dependent on initial conditions
times=1:100
params=c(0.5,0.01,0.015,0.5,0.02,0.025)
y0=c(2,5)
sim=ode(y=y0,times=times,func=lvSim,parms=params)

sim=data.frame(t=sim[,1],N1=sim[,2],N2=sim[,3])
ggplot(sim,aes(x=t,y=N1))+geom_line()+geom_line(data=sim,mapping=aes(x=t,y=N2),col='red')+theme_classic()
