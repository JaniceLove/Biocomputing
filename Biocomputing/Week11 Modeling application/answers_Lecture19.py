#### Examples and answers for Lecture 19 challenges

# example 1
import pandas
import scipy
import scipy.integrate as spint
from plotnine import *

def ddSim(y,t0,r,K):
    N=y[0]
    dY = N*r*(1-N/K)
    return [dY]

params=(0.1,10)
y=[0.01]
times=range(0,600)

log_growth = spint.odeint(func=logistic,y0=y,t=times,args=params)

modelOutput=pandas.DataFrame({"t":times,"N":log_growth[:,0]})

ggplot(modelOutput,aes(x="t",y="N"))+geom_line()

# example 2
def predPreySim(y,t0,r,K,consume,predDeath):
    prey=y[0]
    pred=y[1]
    
    dPrey_dt=r*(1-prey/K)*prey-consume*prey*pred
    dPred_dt=consume*prey*pred-predDeath*pred
    
    return [dPrey_dt,dPred_dt]

y0=[25,1]
times=range(0,500)
params=(0.4,100,0.004,0.3)

out=spint.odeint(func=predPreySim,y0=y0,t=times,args=params)

outDF=pandas.DataFrame({"t":times,"prey":out[:,0],"pred":out[:,1]})

predPreyPlot=ggplot(outDF,aes(x="t",y="prey"))+geom_line()+theme_classic()
predPreyPlot+geom_line(outDF,aes(x="t",y="pred"),color="red")

# Challenge 1
def tumorSim(y,t0,RN,KN,aTN,RT,KT,aNT):
    N=y[0]
    T=y[1]
    
    dNdt=RN*(1-(N+aNT*T)/KN)*N
    dTdt=RT*(1-(T+aTN*N)/KT)*T
    
    return [dNdt,dTdt]

# case 2
times=range(0,100)
y0=[0.1,0.1]
params=(0.1,10,2,0.1,10,0.5)
sim=spint.odeint(func=tumorSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"normal":sim[:,0],"tumor":sim[:,1]})
ggplot(simDF,aes(x="t",y="normal"))+geom_line()+geom_line(simDF,aes(x="t",y="tumor"),color='red')+theme_classic()

# case 3
times=range(0,500)
y0=[0.05,0.2]
params=(0.1,10,0.5,0.1,10,0.5)
sim=spint.odeint(func=tumorSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"normal":sim[:,0],"tumor":sim[:,1]})
ggplot(simDF,aes(x="t",y="normal"))+geom_line()+geom_line(simDF,aes(x="t",y="tumor"),color='red')+theme_classic()

# case 4
times=range(0,100)
y0=[0.1,0.1]
params=(0.1,10,0.5,0.1,10,2)
sim=spint.odeint(func=tumorSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"normal":sim[:,0],"tumor":sim[:,1]})
ggplot(simDF,aes(x="t",y="normal"))+geom_line()+geom_line(simDF,aes(x="t",y="tumor"),color='red')+theme_classic()


# Challenge 2
# 2 state variables and 2 differential equations
# dN1dt=r1*(1-alpha11*N1-alpha12*N2)*N1
# dN2dt=r2*(1-alpha22*N2-alpha21*N1)*N2

def lvSim(y,t0,r1,alpha11,alpha21,r2,alpha22,alpha12):
    N1=y[0]
    N2=y[1]
    
    dN1dt=r1*(1-alpha11*N1-alpha12*N2)*N1
    dN2dt=r2*(1-alpha22*N2-alpha21*N1)*N2
    
    return [dN1dt,dN2dt]

# coexistence - alpha12<alpha11 & alpha21<alpha22
times=range(0,100)
params=(0.5,0.01,0.005,0.5,0.02,0.005)
y0=[2,2]
sim=spint.odeint(func=lvSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"N1":sim[:,0],"N2":sim[:,1]})
ggplot(simDF,aes(x="t",y="N1"))+geom_line()+geom_line(simDF,aes(x="t",y="N2"),color="red")+theme_classic()

# competitive exclusion by N1 - alpha21 > alpha11
times=range(0,100)
params=(0.5,0.01,0.015,0.5,0.02,0.005)
y0=[2,2]
sim=spint.odeint(func=lvSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"N1":sim[:,0],"N2":sim[:,1]})
ggplot(simDF,aes(x="t",y="N1"))+geom_line()+geom_line(simDF,aes(x="t",y="N2"),color="red")+theme_classic()

# competitive exclusion by N2 - alpha12 > alpha22
times=range(0,100)
params=(0.5,0.01,0.005,0.5,0.02,0.025)
y0=[2,2]
sim=spint.odeint(func=lvSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"N1":sim[:,0],"N2":sim[:,1]})
ggplot(simDF,aes(x="t",y="N1"))+geom_line()+geom_line(simDF,aes(x="t",y="N2"),color="red")+theme_classic()

# competitive exclusion dependent on initial conditions
times=range(0,100)
params=(0.5,0.01,0.015,0.5,0.02,0.025)
y0=[5,2]
sim=spint.odeint(func=lvSim,y0=y0,t=times,args=params)
simDF=pandas.DataFrame({"t":times,"N1":sim[:,0],"N2":sim[:,1]})
ggplot(simDF,aes(x="t",y="N1"))+geom_line()+geom_line(simDF,aes(x="t",y="N2"),color="red")+theme_classic()
