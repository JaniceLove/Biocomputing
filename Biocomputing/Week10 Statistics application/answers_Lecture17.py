# Answers from Challenge for maximum likelihood estimation - Lecture 17

# estimate parameters for the candidate models presented in Table 2 of Kelly et al.
# candidate explanatory variables for zooplankton production include:
# Kd (light attenuation)
# DOC (dissolved organic carbon concentration)
# chlorophyll a (algal biomass)
# piscivore (presence or absence of predatory fish)
# TP (total phosphorus concentration)
# chaoborus density

### import packages
import numpy
import pandas
from scipy.optimize import minimize
from scipy.stats import norm

# load data
data=pandas.read_csv("kelly2014Data.csv",header=0,index_col=0)

# all models are linear models, so we can define one likelihood function
def nllike(p,obs):
    B0=p[0]
    B1=p[1]
    sigma=p[2]
    
    expected=B0+B1*obs.x
    nll=-1*norm(expected,sigma).logpdf(obs.y).sum()
    return nll

initialGuess=numpy.array([50,-1,1])

# manual way
# Kd
dataIn=pandas.DataFrame({"x":data.Kd,"y":data.zoopProduction})
kd_fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
kd_fit
# DOC
dataIn=pandas.DataFrame({"x":data.DOC,"y":data.zoopProduction})
DOC_fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
DOC_fit
# chlorophyll a
dataIn=pandas.DataFrame({"x":data.chlorophyll,"y":data.zoopProduction})
chl_fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
chl_fit
# piscivore
dataIn=pandas.DataFrame({"x":1*(data.piscivores=="present"),"y":data.zoopProduction})
pisc_fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
pisc_fit
# TP
dataIn=pandas.DataFrame({"x":data.totalPhosphorous,"y":data.zoopProduction})
TP_fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
TP_fit
# chaoborus
dataIn=pandas.DataFrame({"x":data.chaoborus,"y":data.zoopProduction})
chaob_fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
chaob_fit


# using a loop
# rearrange data to match order of Table 2
data2=pandas.DataFrame({"Kd":data.Kd,"DOC":data.DOC,"chlorophyll":data.chlorophyll,"pisc":data.piscivores,"TP":data.totalPhosphorous,"chaob":data.chaoborus,"zoopProd":data.zoopProduction})

modelOutput=pandas.DataFrame(numpy.zeros((6,3)),columns=["B0","B1","NLL"])

for i in range(0,5):
    dataIn=pandas.DataFrame({"x":data2.iloc[:,i],"y":data2.zoopProd})
    fit=minimize(nllike,initialGuess,method="Nelder-Mead",args=dataIn)
    
    modelOutput.iloc[i,0]=fit.x[0]
    modelOutput.iloc[i,1]=fit.x[1]
    modelOutput.iloc[i,2]=fit.fun
