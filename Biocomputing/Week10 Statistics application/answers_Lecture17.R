# Answers from Challenge for maximum likelihood estimation - Lecture 17

# estimate parameters for the candidate models presented in Table 2 of Kelly et al.
# candidate explanatory variables for zooplankton production include:
# Kd (light attenuation)
# DOC (dissolved organic carbon concentration)
# chlorophyll a (algal biomass)
# piscivore (presence or absence of predatory fish)
# TP (total phosphorus concentration)
# chaoborus density

# load data
data=read.csv("kelly2014Data.csv",header=TRUE,stringsAsFactors=FALSE,row.names=1)

# all models are linear models, so we can define one likelihood function
nllike<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  
  expected=B0+B1*x
  
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}

initialGuess=c(30,-1,1)

# slow manual way
# Kd
Kd_fit=optim(par=initialGuess,fn=nllike,y=data$zoopProduction,x=data$Kd)
Kd_fit
# DOC
DOC_fit=optim(par=initialGuess,fn=nllike,y=data$zoopProduction,x=data$DOC)
DOC_fit
# chlorophyll a
chl_fit=optim(par=initialGuess,fn=nllike,y=data$zoopProduction,x=data$chlorophyll)
chl_fit
# piscivore
pisc_fit=optim(par=initialGuess,fn=nllike,y=data$zoopProduction,x=(data$piscivores=="present")*1)
pisc_fit
# TP
TP_fit=optim(par=initialGuess,fn=nllike,y=data$zoopProduction,x=data$totalPhosphorous)
TP_fit
# chaoborus
chaob_fit=optim(par=initialGuess,fn=nllike,y=data$zoopProduction,x=data$chaoborus)
chaob_fit


# using a loop
# rearrange data to match order of Table2
data=data[,c(3,2,4,7,5,6,1)]
data$piscivores=(data$piscivores=="present")*1

modelOutput=matrix(NA,6,3)
for(i in 1:nrow(modelOutput)){
  fit=optim(par=initialGuess,fn=nllike,x=data[,i],y=data[,7])
  modelOutput[i,]=c(fit$par[1:2],fit$value)
}

rownames(modelOutput)=colnames(data)[1:6]
colnames(modelOutput)=c("B0","B1","NLL")

modelOutput
