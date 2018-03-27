###### PS8 Data Sci

# Task 4: create a data set with the specified properties

## set seed
set.seed(100)

## create random x matrix
N = 100000
K = 10
sigma = 0.5
X = matrix(rnorm( N*K, mean = 0, sd = 1), N , K)
X[,1] = 1
head(X)

## create error term
eps = rnorm(N, mean = 0, sd = 0.5)
head(eps)

## create Beta
beta = c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25 , 2)

## generate Y vector
Y = X %*% beta + eps
head(Y)
estimates = lm(Y~X -1)
print(summary(estimates))

# Task 5: compute beta^ OLS using the closed form solution

## closed form model
betaH = round(solve(t(X)%*%X)%*%t(X)%*%Y, digits=4)
print(betaH)

## print results
closeF = betaH
print(closeF)

# Task 6: compute using gradient descent

## stepsize
alpha <- 0.0000003
maxiter <- 500000

## define objective function
objfun <- function(beta,Y,X) {
  return ( sum((Y-X%*%beta)^2) )
}

## define gradient function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## define data
Y <- Y
X <- X
beta <- runif(dim(X)[2])

## set seed
set.seed(100)

## create beta vector
beta.All <- matrix("numeric",length(beta),maxiter)

## gradient descent function 
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

## print result and plot all xs for every iteration
print(iter)
gradR = beta
print(gradR)


# Task 7.1: solve using L_BFGS Alg.

## install nloptr
if(!require(nloptr)) install.packages("nloptr")

## create data
Y = Y
X = X
beta0 = runif(dim(X)[2])

## define objective function 
obj = function(beta, Y, X){
  return(sum(Y-X%*%beta)^2)
}

## define gradient function 
grad = function(beta, Y, X){
  return(as.vector(-2*t(X)%*%(Y-X%*%beta)))
}
  
## options for alg.  
options = list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## optimizing using L_BFGS
LBFG = nloptr(x0 = beta0, eval_f = obj, eval_grad_f = grad, opts = options, Y = Y, X = X)
LBFG = as.vector(LBFG)
print(LBFG)

# Task 7.2: solve using Nelder-Mead Alg. 

## install nloptr if needed 
#if(!require(nloptr)) install.packages("nloptr")

## define objective function 
obj  <- function(theta,Y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

## define gradient 
gradient <- function (theta ,Y,X) {
  grad <- as.vector ( rep (0, length ( theta )))
  beta <- theta [1:( length ( theta ) -1)]
  sig <- theta [ length ( theta )]
  grad [1:( length ( theta ) -1)] <- -t(X)%*%(Y - X%*% beta )/( sig ^2)
  grad [ length ( theta )] <- dim (X) [1] /sig - crossprod (Y-X%*% beta )/( sig^3)
  return ( grad )
}

## create data
Y = Y
X = X
beta0 = runif(dim(X)[2]+1)

## options for alg.  
options = list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## optimizing using Nelder-Mead
result = nloptr( x0=beta0,eval_f=obj,opts=options,Y=Y,X=X)
NM  <- result$solution[1:(length(result$solution)-1)]
print(NM)

# Task 8: Compute Beta MLE using nloptr's L_BFGS algorithm. 

## gradient vector

  objfun  <- function(theta,Y,X) {
    # need to slice our parameter vector into beta and sigma components
    beta    <- theta[1:(length(theta)-1)]
    sig     <- theta[length(theta)]
    # write objective function as *negative* log likelihood (since NLOPT minimizes)
    loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
    return (loglike)
  }
  
  gradient <- function (theta ,Y,X) {
    grad <- as.vector ( rep (0, length ( theta )))
    beta <- theta [1:( length ( theta ) -1)]
    sig <- theta [ length ( theta )]
    grad [1:( length ( theta ) -1)] <- -t(X)%*%(Y - X%*% beta )/( sig ^2)
    grad [ length ( theta )] <- dim (X) [1] /sig - crossprod (Y-X%*% beta )/( sig
                                                                              ^3)
    return ( grad )
  }
  
## create data
Y = Y
X = X

## create initial values
beta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
#theta0 <- append(as.vector(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris))$coefficients[,1]),runif(1))

options = list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## optimizing using L_BFGS
LBFG2 = nloptr(x0 = beta0, eval_f = objfun, eval_grad_f =gradient , opts = options, Y = Y, X = X)
print(LBFG2)

# Task 9: Use LM and directly call the matrices

## simple Linear Regression model 
lmRes = lm(Y~X - 1)
print(lmRes)

## Task 9.2: Use stargazer to export results
if(!require(stargazer)) install.packages("stargazer")
stargazer(lmRes)
