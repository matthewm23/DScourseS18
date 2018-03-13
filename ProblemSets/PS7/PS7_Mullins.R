# Problem Set 7

#set directory
setwd("~/Documents/General Paperwork/Spring 2018 Classes/BayesStats")

#read in data
mydata = read.csv("wageData.csv")
data = as.data.frame(mydata)
colnames(data) = c("logwage", "hgc", "college", "tenure", "age", "married")
data[data == ""] = NA
summary(data)
head(data)

#use mice to make better observation
if(!require(mice)) install.packages("mice")
md.pattern(data)

#drop observations where hgc or tenure are missing
if(!require(tidyr)) install.packages("tidyr")
ClnData = data %>% drop_na(hgc, tenure)
summary(ClnData)

#find factors
sapply(ClnData, function(x) is.factor(x))

#convert to binary
ClnData$college = as.integer(ClnData$college != "not college grad")
ClnData$married = as.integer(ClnData$married != "single")
head(ClnData)

#summary of data frame, in LATEX
if(!require(stargazer)) install.packages("stargazer")
stargazer(ClnData)


#### IMP 1. 1 list wise deletion on logwage
#listwise delete log wage 

ClnData1 = ClnData %>% drop_na(logwage)
head(ClnData1)

####linear model 1

#fit variables for lm model
logwage = ClnData1$logwage
hgc = ClnData1$hgc 
college = ClnData1$college 
tenure = ClnData1$tenure
age = ClnData1$age
married = ClnData1$married

if(!require(rjags)) install.packages("rjags")
if(!require(coda)) install.packages("coda")
if(!require(xtable)) install.packages("xtable")
Ntotal = length(logwage)  # Compute the total number of data rows
dataList = list(    # Put the information into a list.
  logwage = logwage,
  hgc = hgc ,
  college = college, 
  tenure = tenure,
  age = age,
  married = married,
  Ntotal = Ntotal 
)

#Define the model:
modelString = "
model{
for(i in 1:Ntotal)
{
  logwage[i] ~ dnorm(mu[i], tau)
  mu[i]<- beta0 + beta1*hgc[i] + beta2*college[i] + beta3*tenure[i] + beta4*pow(tenure[i],2) + beta5 * age[i] + beta6 * married[i]
}

beta0 ~ dnorm(0.0, 1.0E-6)
beta1 ~ dnorm(0.0, 1.0E-6)
beta2 ~ dnorm(0.0, 1.0E-6)
beta3 ~ dnorm(0.0, 1.0E-6)
beta4 ~ dnorm(0.0, 1.0E-6)
beta5 ~ dnorm(0.0, 1.0E-6)
beta6 ~ dnorm(0.0, 1.0E-6)
sigma ~ dunif(0, 1000)
tau <- pow(sigma,  -2)
}
" 
writeLines( modelString , con="TEMPmodel.txt" )

initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, beta4 =0, beta5 = 0,beta6 = 0, sigma =10)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2" ,"beta3", "beta4", "beta5","beta6", "sigma") ,
                            n.iter=1000 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )
s = summary(codaSamples)
s1 = as.mcmc(s)
print(s)

#### IMP 2. Perform mean imputation to fill in missing log wages

#perform mean imputation to fill in missing log wages

NAmean = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
ClnData2 = replace(ClnData, TRUE, lapply(ClnData, NAmean))
head(ClnData2)

####linear model 2

#fit variables for lm model
logwage = ClnData2$logwage
hgc = ClnData2$hgc 
college = ClnData2$college 
tenure = ClnData2$tenure
age = ClnData2$age
married = ClnData2$married

Ntotal = length(logwage)  # Compute the total number of data rows
dataList = list(    # Put the information into a list.
  logwage = logwage,
  hgc = hgc ,
  college = college, 
  tenure = tenure,
  age = age,
  married = married,
  Ntotal = Ntotal 
)

#Define the model:
modelString = "
model{
for(i in 1:Ntotal)
{
  logwage[i] ~ dnorm(mu[i], tau)
  mu[i]<- beta0 + beta1*hgc[i] + beta2*college[i] + beta3*tenure[i] + beta4*pow(tenure[i],2) + beta5 * age[i] + beta6 * married[i]
}

beta0 ~ dnorm(0.0, 1.0E-6)
beta1 ~ dnorm(0.0, 1.0E-6)
beta2 ~ dnorm(0.0, 1.0E-6)
beta3 ~ dnorm(0.0, 1.0E-6)
beta4 ~ dnorm(0.0, 1.0E-6)
beta5 ~ dnorm(0.0, 1.0E-6)
beta6 ~ dnorm(0.0, 1.0E-6)
sigma ~ dunif(0, 1000)
tau <- pow(sigma,  -2)
}
" 
writeLines( modelString , con="TEMPmodel.txt" )

initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, beta4 =0, beta5 = 0,beta6 = 0, sigma =10)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
codaSamples2 = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2" ,"beta3", "beta4", "beta5","beta6", "sigma") ,
                            n.iter=1000 )
save( codaSamples2 , file=paste0(fileNameRoot,"Mcmc.Rdata") )
summary(codaSamples2)


#### IMP 3. Impute means from the complete cases regression above
if(!require(magrittr)) install.packages("magrittr")
if(!require(simputation)) install.packages("simputation")

#impute missing wages 
ClnData3 = ClnData %>% impute_lm(logwage ~ hgc + college + tenure^2 + age + married)
head(ClnData3)

#### linear model 3

#fit variables for lm model
logwage = ClnData3$logwage
hgc = ClnData3$hgc 
college = ClnData3$college 
tenure = ClnData3$tenure
age = ClnData3$age
married = ClnData3$married

Ntotal = length(logwage)  # Compute the total number of data rows
dataList = list(    # Put the information into a list.
  logwage = logwage,
  hgc = hgc ,
  college = college, 
  tenure = tenure,
  age = age,
  married = married,
  Ntotal = Ntotal 
)

#Define the model:
modelString = "
model{
for(i in 1:Ntotal)
{
  logwage[i] ~ dnorm(mu[i], tau)
  mu[i]<- beta0 + beta1*hgc[i] + beta2*college[i] + beta3*tenure[i] + beta4*pow(tenure[i],2) + beta5 * age[i] + beta6 * married[i]
}

beta0 ~ dnorm(0.0, 1.0E-6)
beta1 ~ dnorm(0.0, 1.0E-6)
beta2 ~ dnorm(0.0, 1.0E-6)
beta3 ~ dnorm(0.0, 1.0E-6)
beta4 ~ dnorm(0.0, 1.0E-6)
beta5 ~ dnorm(0.0, 1.0E-6)
beta6 ~ dnorm(0.0, 1.0E-6)
sigma ~ dunif(0, 1000)
tau <- pow(sigma,  -2)
}
" 
writeLines( modelString , con="TEMPmodel.txt" )

initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, beta4 =0, beta5 = 0,beta6 = 0, sigma =10)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
codaSamples3 = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2" ,"beta3", "beta4", "beta5","beta6", "sigma") ,
                            n.iter=1000 )
save( codaSamples3 , file=paste0(fileNameRoot,"Mcmc.Rdata") )
summary(codaSamples3)


#### IMP 4. Use mice package to perform multi imp. regression 

#check data
head(ClnData)

#impute data
ClnData4 = mice(ClnData, seed = 33)

#fit with model 
model4 = with(ClnData4, lm(logwage ~ hgc + college + tenure^2 + age + married))
summary(pool(model4))
