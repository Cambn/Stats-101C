### load datasets from the place you downloaded them from

load("/Users/cliccuser/Downloads/subset_training_lafd.rdata")
load("/Users/cliccuser/Downloads/testing_subset_training_lafd_dummy.rdata")

### clean the dataset
train2 <- subset_training_lafd[complete.cases(subset_training_lafd), ] # remove NAs
names(train2) <- make.names(names(train2), unique = TRUE)
names(testing_subset_training_lafd) <- make.names(names(testing_subset_training_lafd), unique = TRUE)  ## fix the column names
testing_subset_training_lafd <- testing_subset_training_lafd[complete.cases(testing_subset_training_lafd), ]

set.seed(123)
smpl <- sample(1:nrow(train2), .8*nrow(train2))
train <- train2[smpl, ] # sample training set
testing_subset_training_lafd <-  train2[-smpl, ] #sample testing_subset_training_lafding set

smpl2 <- sample(1:nrow(train), 650000)
train3 <- train[sample(train2, 650000), ] # make a smaller train set to run our model on

for (i in 1:ncol(train3)){
  train3[, i] <- as.numeric(train3[,i ])
}

for (i in 1:ncol(testing_subset_training_lafd)){
  testing_subset_training_lafd[, i] <- as.numeric(testing_subset_training_lafd[,i ])
}  # converts everything to numeric values



### xgboost stuff
require(xgboost)
require(mlr)

# make tasks
traintask <- makeRegrTask (data = train3,target = "transformed")
testing_subset_training_lafdtask <- makeRegrTask (data = testing_subset_training_lafd,target = "transformed")

# one hot encoding
traintask <- createDummyFeatures (obj = traintask) 
testing_subset_training_lafdtask <- createDummyFeatures (obj = testing_subset_training_lafdtask)

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

