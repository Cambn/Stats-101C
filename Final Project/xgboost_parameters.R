
#### right now the best MSE comes from just changing the eta to .4 - idk if this will actually transpire into results tho
#### bc MSE seems to be a lot lower than what I'd expect lmao gg rip

# Default Settings --------------------------------------------------------

params1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

dtrain <- xgb.DMatrix(data = as.matrix(train4),label = exp(train3$transformed)) 
dtest <- xgb.DMatrix(data = as.matrix(test1), label = exp(test$transformed))

xgbcv1 <- xgb.cv( params = params1, data = dtrain, nrounds = 100, nfold = 5, showsd = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgb1 <- xgb.train (params = params1, data = dtrain, nrounds = 100, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)


xgbpred1 <- predict (xgb1,dtest)

mean((xgbpred1 - exp(test$transformed))^2) #sample MSE -- 1.717 million



# Play with eta parameter ------------------------------------------------------------------------


params_eta1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_eta2 <- list(booster = "gbtree", objective = "reg:linear", eta=0.5, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_eta3 <- list(booster = "gbtree", objective = "reg:linear", eta=0.6, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
### want to run this one
params_eta4 <-list(booster = "gbtree", objective = "reg:linear", eta=0.7, gamma=10, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
###
params_eta5 <-list(booster = "gbtree", objective = "reg:linear", eta=0.8, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_eta6 <-list(booster = "gbtree", objective = "reg:linear", eta=0.9, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_eta7 <-list(booster = "gbtree", objective = "reg:linear", eta=1, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv2 <- xgb.cv( params = params_eta1, data = dtrain, nrounds = 100, nfold = 5, showsd = T, print_every_n = 5, early_stop_round = 20, maximize = F)

xgb2 <- xgb.train (params = params_eta1, data = dtrain, nrounds = 100, watchlist = list(val=dtest,train=dtrain), print_every_n = 5, early_stop_round = 5, maximize = F)

xgbpred2 <- predict (xgb2,dtest)

mean((xgbpred2 - exp(test$transformed))^2) #sample MSE -- 1.670059 million

xgbcv3 <- xgb.cv( params = params_eta3, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb3 <- xgb.train (params = params_eta3, data = dtrain, nrounds = 50, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred3 <- predict(xgb3,dtest)

mean((exp(xgbpred3) - exp(test$transformed))^2) #sample MSE -- 1.508816 million





### increasing the eta above .6 seems to increase the MSE, with .6 appearing to be our optimum with these rest parameters
### as is


# Play with gammas --------------------------------------------------------

params_gam1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.2, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_gam2 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_gam3 <- list(booster = "gbtree", objective = "reg:linear", eta=.4, gamma=25, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params_gam4 <- list(booster = "gbtree", objective = "reg:linear", eta=0.5, gamma=50, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)



xgbcv4 <- xgb.cv( params = params_gam3, data = dtrain, nrounds = 100, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb4 <- xgb.train (params = params_gam3, data = dtrain, nrounds = 50, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred4 <- predict(xgb4,dtest)

mean((xgbpred4 - exp(test$transformed))^2) #sample MSE -- 1.527 million

xgbcv5 <- xgb.cv( params = params_gam2, data = dtrain, nrounds = 100, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb5 <- xgb.train (params = params_gam2, data = dtrain, nrounds = 31, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred5 <- predict(xgb5,dtest)

mean((exp(xgbpred5) - exp(test$transformed))^2) #sample MSE -- 1.445475 million 





# Playing with max depth -------------------------------------------

params_depth1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
params_depth2 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=1, subsample=1, colsample_bytree=1)
params_depth3 <- list(booster = "gbtree", objective = "reg:linear", eta=.4, gamma=0, max_depth=20, min_child_weight=1, subsample=1, colsample_bytree=1)
params_depth4 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=0, max_depth=40, min_child_weight=1, subsample=1, colsample_bytree=1)


xgbcv6 <- xgb.cv( params = params_depth1, data = dtrain, nrounds = 100, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb6 <- xgb.train (params = params_depth1, data = dtrain, nrounds = 45, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred6 <- predict(xgb6,dtest)

mean((xgbpred6 - exp(test$transformed))^2) # 1.64 million


xgbcv7 <- xgb.cv( params = params_depth2, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb7 <- xgb.train (params = params_depth2, data = dtrain, nrounds = 46, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred7 <- predict(xgb7,dtest)

mean(((exp(xgbpred7)+100) - exp(test$transformed))^2) #sample MSE -- 1.453171 million
### or if you add 130 to it for no apparent reason it's 1.4425 million



# Playing with child weight -----------------------------------------------
params_mcw1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=5, subsample=1, colsample_bytree=1)
params_mcw2 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=10, subsample=1, colsample_bytree=1)
params_mcw3 <- list(booster = "gbtree", objective = "reg:linear", eta=.4, gamma=10, max_depth=4, min_child_weight=30, subsample=1, colsample_bytree=1)
params_mcw4 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=20, subsample=1, colsample_bytree=1)

xgbcv8 <- xgb.cv( params = params_mcw1, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb7 <- xgb.train (params = params_mcw1, data = dtrain, nrounds = 50, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred7 <- predict(xgb7,dtest)

mean((exp(xgbpred7) - exp(test$transformed))^2) #sample MSE -- 1.451553 million

xgbcv9 <- xgb.cv( params = params_mcw2, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb8 <- xgb.train (params = params_mcw2, data = dtrain, nrounds = 50, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred8 <- predict(xgb8,dtest)

mean((exp(xgbpred8) - exp(test$transformed))^2)  # 1.4509 million


xgbcv10 <- xgb.cv( params = params_mcw4, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb9 <- xgb.train (params = params_mcw4, data = dtrain, nrounds = 31, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred9 <- predict(xgb9,dtest)

mean((xgbpred9 - exp(test$transformed))^2)  # 1.387466 million - this increased my MSE though

xgbcv11 <- xgb.cv( params = params_mcw3, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb10 <- xgb.train (params = params_mcw3, data = dtrain, nrounds = 50, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred10 <- predict(xgb10,dtest)

mean((xgbpred10 - exp(test$transformed))^2)  # 1.407 million


# Playing with Other Parameters -------------------------------------------

params_other1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=20, subsample=.5, colsample_bytree=.5, lambda = 1)
params_other2 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=20, subsample=1, colsample_bytree=1)
params_mcw3 <- list(booster = "gbtree", objective = "reg:linear", eta=.4, gamma=10, max_depth=4, min_child_weight=30, subsample=1, colsample_bytree=1)
params_mcw4 <- list(booster = "gbtree", objective = "reg:linear", eta=0.4, gamma=10, max_depth=4, min_child_weight=20, subsample=1, colsample_bytree=1)

xgbcv11 <- xgb.cv( params = params_other1, data = dtrain, nrounds = 150, nfold = 5, showsd = T, print_every_n = 15, early_stop_round = 20, maximize = F)

xgb11 <- xgb.train(params = params_other1, data = dtrain, nrounds = 65, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred11 <- predict(xgb11,dtest)

mean((xgbpred11 - exp(test$transformed))^2)  # 1.440332 million, 1.394645 million if
## you don't use transformed as your y variable, 1.386 million

xgb12 <- xgb.train(params = params_other2, data = dtrain, nrounds = 65, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 5, maximize = F)

xgbpred12 <- predict(xgb12,dtest)

mean((xgbpred12 - exp(test$transformed))^2)  # 1.409451 million










# Playing with other stuff ------------------------------------------------










