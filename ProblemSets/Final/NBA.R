#### Data Analysis - NBA Win Predictions


### Set working directory to correct location
setwd("~/Documents/General Paperwork/Spring 2018 Classes/BayesStats")

#**************************
### Import Data
#**************************
  
if(!require(tidyverse)) install.packages("tidyverse")
data = read_csv("NBA_Data.csv")


### Use MICE to Inpsect Data
#if(!require(mice)) install.packages("mice")
#md.pattern(df)

### Convert to Data Frame
df1 = as.data.frame(data)



#*****************************************************
### Clean Data so That it Can be Used More Easily 
#*****************************************************  
  

### Adding Column Names

colnames(df1) = c("team", 
                 "bigCity", 
                 "spending", 
                 "incomeTax", 
                 "avgTemp", 
                 "manageRank", 
                 "luxuryTax", 
                 "miles", 
                 "fanAttend", 
                 "netRTG", 
                 "ticketPrice",
                 "regWins", 
                 "playoff", 
                 "seed", 
                 "crimeIndex")


### Convert City to binary, 1 = big city, 0 = small city 
df1$bigCity = as.integer(df1$bigCity > 3000000)


### Convert Luxury Tax to binary, 1 = paying tax, 0 = below cap limits
df1$luxuryTax = as.integer(df1$luxuryTax < 0)


### Convert Playoff to Binary, 1 = made playoffs, 0 = did not make playoffs
df1$playoff = as.integer(df1$playoff == "yes")

###### Perform Imputations for the Mean Imputation

#if(!require(magrittr)) install.packages("magrittr")
#if(!require(simputation)) install.packages("simputation")

NAmean = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df = replace(df1, TRUE, lapply(df1, NAmean))
head(df)


### Check Data to make sure everything looks right
#print(df)



#************************************
###### Test for Covariance in Model
#************************************

## Test 1 Luxury Tax and Spending
luxuryTax = df$luxuryTax
spending = df$spending
t.test(spending ~ luxuryTax)
#plot(spending~luxuryTax)




#*****************************
##### Visualize Data to Begin 
#*****************************

if(!require(ggplot2)) install.packages("ggplot2")


#### Graph 1, Net Rating and Playoff Contention
g1 = ggplot(df, aes(x = netRTG, y = playoff ,color = netRTG)) + ylab("Playoffs") + theme_bw() 
g1 = g1 + geom_point(position = position_jitter(height = 0.015, width =0))
g1 = g1 + stat_smooth(method = "glm", method.args = list(family=binomial("logit")), formula = y ~ x + I(x^2) + I(x^3), alpha = 0.25, size = 2.5, aes(fill = netRTG ))
print(g1)

#### Graph 2, Avg Temp, Total Wins, and Playoff Contention
g2 = ggplot(df, aes(x = avgTemp, y = regWins , color = regWins)) + ylab("Regular Season Wins") + theme_bw() 
g2 = g2 + geom_point(position = position_jitter(height = 0.015, width =0))
g2 = g2 + facet_wrap(~playoff)
print(g2)

#### Graph 3, Net Rating, Pays Luxury Tax, Playoffs
g3 = ggplot(df, aes(x = netRTG, y = luxuryTax ,color = netRTG)) + ylab("Pays Luxury Tax") + theme_bw() 
g3 = g3 + geom_point(position = position_jitter(height = 0.015, width =0))
g3 = g3 + stat_smooth(method = "glm", method.args = list(family=binomial("logit")), formula = y ~ x + I(x^2) + I(x^3), alpha = 0.25, size = 2.5, aes(fill = netRTG ))
g3 = g3 + facet_wrap(~playoff)
print(g3)





#****************************************
#### Classical Analysis Using OLS Model 
#****************************************
  
if(!require(vcdExtra)) install.packages("vcdExtra")
if(!require(gpairs)) install.packages("gpairs")
if(!require(dplyr)) install.packages("dplyr")

lm = lm(formula = netRTG ~ 
                  bigCity + 
                  spending + 
                  incomeTax + 
                  avgTemp + 
                  manageRank + 
                  luxuryTax +
                  miles +
                  fanAttend  + 
                  ticketPrice + 
                  crimeIndex,   data = df)

### Results for OLS Model
summary(lm)
confint(lm)
exp(coef(lm))



#**********************
#### Bayesian Model- GIBBS Sampler
#**********************


if(!require(rjags)) install.packages("rjags")               
source("DBDA2E-utilities.R")
fileNameRoot="NBA.Mullins" # For output file names.

#### Create Data List 

  bigCity = df$bigCity
  spending = df$spending
  incomeTax = df$incomeTax
  avgTemp = df$avgTemp
  manageRank = df$manageRank
  luxuryTax = df$luxuryTax
  miles = df$miles
  fanAttend = df$fanAttend 
  netRTG = df$netRTG 
  ticketPrice = df$ticketPrice
    crimeIndex =  df$crimeIndex


Ntotal = length(bigCity)  # Compute the total number of data rows
dataList = list(    # Put the information into a list.
  bigCity = bigCity,
  spending = spending, 
  incomeTax = incomeTax,
  avgTemp = avgTemp, 
  manageRank = manageRank, 
  luxuryTax = luxuryTax, 
  miles = miles,
  fanAttend = fanAttend,
  netRTG = netRTG, 
  ticketPrice = ticketPrice,
  crimeIndex = crimeIndex,
  Ntotal = Ntotal 
)


#### Define the model:

modelString = "
model{
for(i in 1:Ntotal)
{
  
  ### Posterior
  netRTG[i] ~ dnorm(mu[i], tau)
  
  ### Error Term
  mu[i]<- beta0 + beta1*bigCity[i] + beta2*spending[i] + beta3*incomeTax[i] + 
  beta4 * avgTemp[i] + beta5 * manageRank[i] + beta6 * luxuryTax[i] + 
  beta7 * miles[i] + beta8 * fanAttend[i] + beta9 * ticketPrice[i] + beta10 * crimeIndex[i] 
  
}

### Low Impact Priors
beta0 ~ dnorm(0.0, 1.0E-6)
beta1 ~ dnorm(0.0, 1.0E-6)
beta2 ~ dnorm(0.0, 1.0E-6)
beta3 ~ dnorm(0.0, 1.0E-6)
beta4 ~ dnorm(0.0, 1.0E-6)
beta5 ~ dnorm(0.0, 1.0E-6)
beta6 ~ dnorm(0.0, 1.0E-6)
beta7 ~ dnorm(0.0, 1.0E-6)
beta8 ~ dnorm(0.0, 1.0E-6)
beta9 ~ dnorm(0.0, 1.0E-6)
beta10 ~ dnorm(0.0, 1.0E-6)


### Precision Terms 
sigma ~ dunif(0, 1000)
tau <- pow(sigma,  -2)
}

" ### close quote for modelString

writeLines( modelString , con="TEMPmodel.txt" )

initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, beta4=0, beta5=0 , beta6=0 , 
                 beta7=0 , beta8=0 , beta9=0 , beta10 =0, sigma =10)

### Run the chains:

jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList 
      ,n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )

### Create Samples

codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2" ,
  "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "beta9", "beta10" ,"sigma") ,n.iter=33340 )


### Save Results

save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

###Print Initial Results

if(!require(ggmcmc)) install.packages("ggmcmc")
summary(codaSamples)
s = ggs(codaSamples)
ggs_density(s)
ggs_crosscorrelation(s)





#*************************
### Bayesian Diagnostics
#*************************

quartz()
graphics.off()

diagMCMC( codaObject = codaSamples , parName= "beta0" )
saveGraph( file=paste0(fileNameRoot,"beta0conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta1" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta2" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta3" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta4" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta5" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta6" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta7" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta8" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta9" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )

diagMCMC( codaObject = codaSamples , parName= "beta10" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") ,type="eps" )
           
diagMCMC( codaObject = codaSamples , parName= "sigma" )
saveGraph( file=paste0(fileNameRoot,"beta1conv") , type="eps" )




