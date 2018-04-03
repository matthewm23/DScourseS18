#### Problem Set #9

##Install Packages for Lab
install.packages("mlr")
install.packages("glmnet")

## Load Housing Data
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

## Add New Features
housing $ lmedv <- log ( housing $ medv )
housing $ medv <- NULL # drop median value
formula <-  as.formula ( lmedv ~ .^3 +
                           poly (crim , 6) +
                           poly (zn , 6) +
                           poly (indus , 6) +
                           poly (nox , 6) +
                           poly (rm , 6) +
                           poly (age , 6) +
                           poly (dis , 6) +
                           poly (rad , 6) +
                           poly (tax , 6) +
                           poly (ptratio , 6) +
                           poly (b , 6) +
                           poly (lstat , 6))
                         

mod_matrix <- data.frame( model.matrix( formula , housing ))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix [, 1] = housing$lmedv
colnames(mod_matrix )[1] = "lmedv" 
head ( mod_matrix ) 

## Break up the data 
n = nrow ( mod_matrix )
train = sample(n, size = .8*n)
test = setdiff (1:n, train )
housing.train = mod_matrix[train ,]
housing.test = mod_matrix[test , ]
head(housing.train)

nrow(housing.train)
ncol(housing.train)

#### Task 6 LASSO to Predict Log Median House Value

if(!require(glmnet)) install.packages("glmnet")
if(!require(mlr)) install.packages("mlr")

## The Task
theTask = makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(theTask)

## Resample Strategy
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

## Prediction Algorithm
predAlg = makeLearner("regr.glmnet")   

## Search over lambda & force elastic net P to be be 1 (LASSO)
param = makeParamSet(makeNumericParam("lambda", lower = 0, upper = 1), makeNumericParam("alpha", lower = 1, upper = 1))

## Take Random Guesses
tune = makeTuneControlRandom(maxit = 50L)

## Perform Tuning
tuneM = tuneParams(learner = predAlg,
                    task = theTask, 
                    resampling = resampleStrat,
                    measures = rmse,
                    par.set = param,
                    control = tune,
                    show.info = TRUE)

## Apply the optimal algorithm parameters to the model
predAlg = setHyperPars(learner=predAlg, par.vals = tuneM$x)

## Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

## Train the final model
finalModel = train(learner = predAlg, task = theTask)

## Predict in test set!
prediction = predict(finalModel, newdata = housing.test)

print(head(prediction$data))

## Out of sample Performance
performance(prediction, measures = list(rmse))

#### Task 7. Use Ridge Estimation

Params = makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

## Resample Strategy
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

## Prediction Algorithm
predAlg = makeLearner("regr.glmnet") 

# Do the tuning again
tunedModel = tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = Params,
                         control = tune,
                         show.info = TRUE)

## Apply the optimal algorithm parameters to the model
predAlg = setHyperPars(learner=predAlg, par.vals = tunedModel$x)

## Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

## Train the final model
finalModel = train(learner = predAlg, task = theTask)

## Predict
prediction2 = predict(finalModel, newdata = housing.test)
print(head(prediction2$data))

## Out of sample Performance
performance(prediction2, measures = list(rmse))

#### Task 8. Elastic Net Model

## Resample Strategy
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

## Prediction Algorithm
predAlg = makeLearner("regr.glmnet") 
Params = makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Do the tuning again
tunedModel = tuneParams(learner = predAlg,
                        task = theTask,
                        resampling = resampleStrat,
                        measures = rmse,       # RMSE performance measure, this can be changed to one or many
                        par.set = Params,
                        control = tune,
                        show.info = TRUE)

## Apply the optimal algorithm parameters to the model
predAlg = setHyperPars(learner=predAlg, par.vals = tunedModel$x)

## Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

## Train the final model
finalModel = train(learner = predAlg, task = theTask)

## Predict
prediction3 = predict(finalModel, newdata = housing.test)
print(head(prediction3$data))

## Out of sample Performance
performance(prediction3, measures = list(rmse))


