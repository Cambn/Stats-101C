load("Kitu/College/Junior Year/Spring Quarter/Stats 101C/dummy_lafd.rdata")
load("Kitu/College/Junior Year/Spring Quarter/Stats 101C/test_dummy.rdata")



sum(!complete.cases(dummy_lafd))
sum(is.na(dummy_lafd$transformed))
median(exp(dummy_lafd$transformed), na.rm = TRUE)



# impute the median value for the missing value NAs
sum(is.na(dummy_lafd2$transformed))
sum(!complete.cases(dummy_lafd))
### clean the dataset

train2 <- dummy_lafd[complete.cases(dummy_lafd), ] # remove predictor rows with NAs
colnames(train2) <- make.names(colnames(train2), unique = TRUE)

colnames(test_dummy) <- make.names(colnames(test_dummy), unique = TRUE)  ## fix the column names



set.seed(123)
smpl <- sample(1:nrow(train2), .8*nrow(train2))
train <- train2[smpl, ] # training set
test <-  train2[-smpl, ] #sample testing set

smpl2 <- sample(1:nrow(train), 1425000)
train3 <- train[smpl2, ] # make a smaller train set to run our model on

for (i in 1:ncol(train)){
  train3[, i] <- as.numeric(train3[,i ])
}

for (i in 1:ncol(test)){
  test[, i] <- as.numeric(test[,i ])
}  # converts everything to numeric values

test1 <- test[, colnames(test) != "transformed"] # removes response from analysis

train4 <- train3[, colnames(train3)!= "transformed"] # removes response from training analysis

### xgboost stuff
require(xgboost)
require(mlr)

# make tasks
traintask <- makeRegrTask (data = train3,target = "transformed")
testtask <- makeRegrTask (data = test,target = "transformed")

# one hot encoding
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

# create learner and set parameter space
lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="reg:linear", eval_metric="error", nrounds=50L, eta=0.1)
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

# resampling strategy and search strategy
rdesc <- makeResampleDesc("CV",iters=5L)
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

# the thing pray to god 
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)

### have to figure out the measures argument

